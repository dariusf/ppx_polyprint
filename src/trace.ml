
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

open Util
open Util.Untyped

module Environment = struct
  (** Information passed across traversals *)

  (** Names of annotated functions that were successfully transformed.Built up
      during AST traversal. Stateful, and thus mappers here are not reentrant. *)
  let transformed_function_names = ref ([] : string list)

  (** Configuration module path provided for each function name *)
  module NameConfigMap = Map.Make(String)
  let configuration_modules = ref NameConfigMap.empty

  (** The default module to use if not explicitly given *)
  let specified_default_module = ref (None : string list option)
end

module Config = struct

  type t = {
    module_prefix : string list;
    vars : (string) list
  }

  let rec list_of_sequence seq =
    match seq with
    | { pexp_desc = Pexp_sequence (e, f) } -> e :: list_of_sequence f
    | _ -> [seq]

  let default_config = {
    module_prefix = [Names.runtime; Names.default_module];
    vars = [];
  }

  (** Indirection for getting the default module in the face of stateful
      configuration variables. Should be removed when this extension can be
      parameterised by it *)
  let default_module () =
    otherwise default_config.module_prefix !Environment.specified_default_module

  let rec interpret_one e config =
    match e with
    | { pexp_desc = Pexp_construct ({ txt = Lident name }, None) } ->
      (* a module *)
      { config with module_prefix = [name] }
    | { pexp_desc = Pexp_construct ({ txt = path }, None) } ->
      (* a qualified module *)
      { config with module_prefix = longident_to_list path }
    | { pexp_desc = Pexp_tuple ts } ->
      (* tuples are interchangeable with sequences for the most part,
         but sequences may not be nested inside them *)
      List.fold_right interpret_one ts config
    | { pexp_desc = Pexp_ident { txt = Lident name } } ->
      (* a variable name *)
      { config with vars = name :: config.vars }
    | _ -> config

  let interpret attrs =
    match attrs with
    | [] -> { default_config with module_prefix = default_module () }
    | x :: _ -> (* TODO consider all, not just first *)
      begin match x with
        | _, PStr [{pstr_desc = Pstr_eval (seq, _)}] ->
          let config_fields = list_of_sequence seq in
          List.fold_right interpret_one config_fields
            { default_config with module_prefix = default_module () }
        | _ -> { default_config with module_prefix = default_module () }
      end
end

let has_attr name attr =
  match attr with
  | { txt = n }, _ when n = name -> true
  | _ -> false

let extract_binding_info config b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in

  (* Collect info to add to the environment *)
  let open Config in
  let open Environment in
  push fn_name transformed_function_names;
  configuration_modules :=
    NameConfigMap.add fn_name config.module_prefix !configuration_modules;

  (original_rhs, fn_name, params)

let count_params fn_name params =
  let actual = List.length params in
  let clamped = clamp 0 7 actual in
  if clamped <> actual then Errors.warn_too_many_params fn_name actual;
  clamped

(** We only take parameters within the list, but if it is empty,
    we don't filter at all. *)
let filter_params names params =
  match names with
  | [] -> params
  | _ ->
    let p = function
      | Unit -> true
      | Param p -> List.mem p names
    in
    List.filter p params

let run_invocation fn_name params config fn =
  let open Config in
  let filtered_params = filter_params config.vars params in
  let filtered_param_count = List.length filtered_params in
  let run_fn_name = ident_dot
      (config.module_prefix @ [Names.run_n filtered_param_count]) in
  let final_fn =
    if filtered_param_count = List.length params then fn
    else
      fun_wildcards filtered_param_count (app fn (List.map param_to_expr params))
  in
  let invocation = app run_fn_name (List.concat [
      [str fn_name];
      List.map (fun p -> Exp.tuple [
          str (show_param p);
          [%expr PolyPrint.show];
          param_to_expr p
        ]) filtered_params;
      [[%expr PolyPrint.show]];
      [final_fn]])
  in
  invocation

let transform_binding_recursively config b =
  let open Config in
  let (original_rhs, fn_name, params) = extract_binding_info config b in
  ignore (count_params fn_name params);
  let nonrec_body =
    let nonrec_params = Param (Names.self fn_name) :: params in
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name (Names.self fn_name) in
    let new_body = mapper.expr mapper body in
    fun_with_params nonrec_params new_body
  in
  let new_rhs = fun_with_params params [%expr
      let [%p pat_var (Names.mangle fn_name)] = [%e nonrec_body] in
      let rec aux =
        [%e fun_with_params params
              (run_invocation fn_name params config
                 (app (ident (Names.mangle fn_name)) [ident "aux"]));
        ] in [%e app_variables "aux" params]
    ] in
  { b with pvb_expr = new_rhs }

let transform_binding_nonrecursively config b =
  let open Config in
  let (original_rhs, fn_name, params) = extract_binding_info config b in
  ignore (count_params fn_name params);
  let new_rhs =
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name (Names.mangle fn_name) in
    let new_body = mapper.expr mapper body in
    fun_with_params params new_body
  in
  (* let rec is used here when transforming recursive functions.
     Non-recursive functions wouldn't have recursive references,
     so this is safe. *)
  let new_rhs' = fun_with_params params [%expr
      let rec [%p pat_var (Names.mangle fn_name)] = [%e new_rhs] in
      [%e run_invocation fn_name params config
            (ident (Names.mangle fn_name))]]
  in
  { b with pvb_expr = new_rhs' }

let transform_recursive_binding attrs b =
  let config = Config.interpret attrs in
  if any (has_attr "tracerec") attrs then
    transform_binding_recursively config b
  else
    transform_binding_nonrecursively config b

let transform_nonrecursive_binding attrs b =
  let config = Config.interpret attrs in
  transform_binding_nonrecursively config b

let interesting_expr_binding rec_flag attrs binding =
  let has_attribute =
    match rec_flag with
    | Nonrecursive ->
      if any (has_attr "tracerec") attrs then
        Errors.tracerec_used_on_nonrecursive_binding ()
      else
        any (has_attr "trace") attrs
    | Recursive ->
      any (has_attr "trace") attrs || any (has_attr "tracerec") attrs
  in has_attribute && is_function_binding binding

let interesting_str_binding rec_flag binding =
  let attrs = binding.pvb_attributes in
  interesting_expr_binding rec_flag attrs binding

let transform_expr rec_flag transform expr mapper bindings body =
  let change b =
    if interesting_expr_binding rec_flag expr.pexp_attributes b then
      transform expr.pexp_attributes b
    else
      { b with pvb_expr = mapper.expr mapper b.pvb_expr }
  in
  let new_bindings = List.map change bindings in
  { expr with pexp_desc = Pexp_let (rec_flag, new_bindings, mapper.expr mapper body) }

let transform_str rec_flag transform mapper bindings =
  let change b =
    if interesting_str_binding rec_flag b then
      transform b.pvb_attributes b
    else
      { b with pvb_expr = mapper.expr mapper b.pvb_expr }
  in
  let new_bindings = List.map change bindings in
  item @@ Pstr_value (rec_flag, new_bindings)

let check_for_annotation item =
  match item with
  | { pstr_desc = Pstr_attribute ({ txt = "polyprint" }, PStr [{
      pstr_desc = Pstr_eval ({
          pexp_desc = Pexp_construct ({ txt = path }, None)}, _) }]) } ->
      Environment.specified_default_module := Some (longident_to_list path)
  | _ -> ()

(** A mapper that generates tracing boilerplate for annotated functions.
    The traversal is also used to collect information to build up the environment
    for future traversals. *)
let annotation_mapper =
  { default_mapper with
    expr =
      begin
        fun mapper expr ->
          match expr with
          | { pexp_desc = Pexp_let (rec_flag, bindings, body) } ->
            begin
              match rec_flag with
              | Recursive ->
                transform_expr Recursive
                  transform_recursive_binding expr mapper bindings body
              | Nonrecursive ->
                transform_expr Nonrecursive
                  transform_nonrecursive_binding expr mapper bindings body
            end
          | x -> default_mapper.expr mapper x;
      end;
    structure_item = fun mapper item ->
      check_for_annotation item;
      match item with
      | { pstr_desc = Pstr_value (rec_flag, bindings) } ->
        begin
          match rec_flag with
          | Recursive ->
            transform_str Recursive
              transform_recursive_binding mapper bindings
          | Nonrecursive ->
            transform_str Nonrecursive
              transform_nonrecursive_binding mapper bindings
        end
      | s -> default_mapper.structure_item mapper s
  }

(** A mapper that wraps call sites of annotated functions based on the
    environment computed so far *)
let call_wrapping_mapper =
  { default_mapper with
    expr = fun mapper expr ->
      let open Config in
      let open Environment in
      match expr with
      | { pexp_desc =
            Pexp_apply
              ({ pexp_desc = Pexp_ident { txt = Lident fn_name; loc } } as fn, args) }
        when List.mem fn_name !transformed_function_names ||
             List.mem (Names.unself fn_name) !transformed_function_names ->

        let n = count_params fn_name args in
        let module_prefix =
          try
            NameConfigMap.find fn_name !configuration_modules
          with Not_found ->
          try
            NameConfigMap.find (Names.unself fn_name) !configuration_modules
          with Not_found -> default_module ()
        in
        Exp.apply
          (ident_dot (module_prefix @ [Names.call_n n]))
          (("", str fn_name) ::
           ("", Exp.tuple
              [Exp.ident ~loc { txt = Lident "__FILE__"; loc = loc };
               Exp.ident ~loc { txt = Lident "__LINE__"; loc = loc }]) ::
           ("", fn) :: args)

      | _ -> default_mapper.expr mapper expr
  }

