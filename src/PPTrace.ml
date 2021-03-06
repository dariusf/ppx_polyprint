
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

open PPUtil
open PPUtil.Untyped

(** Given a binding, pulls out the relevant fields.
    Also collects information to add to the environment. *)
let extract_binding_info config b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in

  (* Collect information *)
  let open PPEnv in
  configuration_modules :=
    NameConfigMap.add fn_name config.PPConfig.module_prefix !configuration_modules;

  (original_rhs, fn_name, params)

(** Ensures that there are not too many parameters. Warns if
    that is the case. *)
let count_params fn_name params =
  let actual = List.length params in
  let clamped = clamp 0 7 actual in
  if clamped <> actual then Errors.warn_too_many_params fn_name actual;
  clamped

(** We only take parameters within the list, but if it is empty,
    we don't filter at all. *)
let filter_params interesting params =
  match interesting with
  | [] -> params
  | _ ->
      let p = function
        | Unit -> true
        | Param p -> List.mem p interesting
      in
      List.filter p params

let check_all_occur_in given actual =
  let check (name, inside) =
    if not inside then
      Printf.sprintf "%s is not a valid parameter (expecting one of %s)"
        name (string_of_list show_param actual)
      |> failwith
  in
  List.map (fun x -> List.mem (Param x) actual) given
  |> zip given |> List.iter check

(** Generates the invocation which actually runs the function being traced. *)
let run_invocation ~loc fn_name params config fn =
  check_all_occur_in config.PPConfig.vars params;
  let filtered_params = filter_params config.PPConfig.vars params in
  let filtered_param_count = List.length filtered_params in
  assert (filtered_param_count <= List.length params);
  let run_fn_ident =
    Exp.send ~loc
      (qualified_ident ~loc (config.PPConfig.module_prefix @ [Names.config_obj]))
      (Names.run_n filtered_param_count) in
  let final_fn =
    if filtered_param_count = List.length params
    then fn
    else
      fun_wildcards ~loc
        filtered_param_count
        (app ~loc fn (List.map (param_to_expr ~loc) params)) in
  let invocation = app ~loc run_fn_ident (List.concat [
      [str ~loc fn_name];
      List.map (fun p -> Exp.tuple ~loc [
          str ~loc (show_param p);
          [%expr PolyPrint.show];
          param_to_expr ~loc p
        ]) filtered_params;
      [[%expr PolyPrint.show]];
      [final_fn]]) [@metaloc loc]
  in
  invocation

let traced_fn ~loc arity f =
  let name = Names.traced_n arity in
  Exp.construct ~loc ({ txt = Ldot (Lident Names.runtime, name); loc }) (Some f)

let transform_binding_recursively config b =
  let original_rhs, fn_name, params = extract_binding_info config b in
  let arity = count_params fn_name params in
  let nonrec_body, loc =
    let nonrec_params = Param (Names.self fn_name) :: params in
    let body, loc = get_fn_body original_rhs in
    let mapper = app_mapper fn_name (Names.self fn_name) in
    let new_body = mapper.expr mapper body in
    fun_with_params ~loc nonrec_params new_body, loc
  in
  let new_rhs = traced_fn ~loc arity @@ fun_with_params ~loc params [%expr
      let [%p pat_var (Names.mangle fn_name)] = [%e nonrec_body] in
      let rec aux =
        [%e traced_fn ~loc arity @@ fun_with_params ~loc params
              (run_invocation ~loc fn_name params config
                 (app ~loc (ident (Names.mangle fn_name))
                    [eta_abstract ~loc arity (ident ~loc "aux")]))
        ] in [%e app_variables ~loc "aux" params]
    ] [@metaloc loc] in
  { b with pvb_expr = new_rhs }

let transform_binding_nonrecursively config b =
  let original_rhs, fn_name, params = extract_binding_info config b in
  let arity = count_params fn_name params in
  let new_rhs, loc =
    let body, loc = get_fn_body original_rhs in
    let mapper = app_mapper fn_name (Names.mangle fn_name) in
    let new_body = mapper.expr mapper body in
    fun_with_params ~loc params new_body, loc
  in
  (* let rec is used here when transforming recursive functions.
     Non-recursive functions wouldn't have recursive references,
     so this is safe. *)
  let new_rhs' = traced_fn ~loc arity @@ fun_with_params ~loc params [%expr
      let rec [%p pat_var (Names.mangle fn_name)] = [%e new_rhs] in
      [%e run_invocation ~loc fn_name params config
            (ident (Names.mangle fn_name))]] [@metaloc loc]
  in
  { b with pvb_expr = new_rhs' }

let transform_recursive_binding attrs b =
  let config = PPConfig.interpret attrs in
  if any (has_attr "tracerec") attrs then
    transform_binding_recursively config b
  else
    transform_binding_nonrecursively config b

let transform_nonrecursive_binding attrs b =
  let config = PPConfig.interpret attrs in
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
  | { pstr_desc = Pstr_attribute ({ txt = name }, PStr str_inputs) }
    when name = Names.default_annotation ->
      let check item =
        match item with
        | { pstr_desc = Pstr_eval ({
            pexp_desc = Pexp_construct ({ txt = path }, None)}, _) } ->
            (* TODO if there are multiple specified, which does this use? *)
            PPEnv.specified_default_module := Some (longident_to_list path)
        | _ -> ()
      in
      List.iter check str_inputs;
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

