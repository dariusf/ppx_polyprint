
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

open PolyPrintUtil
open PolyPrintUtil.Untyped

let mangle fn_name =
  fn_name ^ "_original"

let params_out_of_range name count =
  failwith @@ Printf.sprintf "ppx_polyprint only supports functions with between 1 and 7 parameters, but %s was given %d" name count

let tracerec_used_on_nonrecursive_binding () =
  failwith @@ Printf.sprintf "[@tracerec] cannot be used on a non-recursive binding"

module TraceConfig = struct

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

  let rec interpret_one e config =
    match e with
    | { pexp_desc = Pexp_construct ({ txt = Lident name }, None) } ->
      (* a module *)
      { config with module_prefix = [name] }
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
    | [] -> default_config
    | x :: _ -> (* TODO consider all, not just first *)
      begin match x with
        | _, PStr [{pstr_desc = Pstr_eval (seq, _)}] ->
          let config_fields = list_of_sequence seq in
          List.fold_right interpret_one config_fields default_config
        | _ -> default_config
      end
end

let has_attr name attr =
  match attr with
  | { txt = n }, _ when n = name -> true
  | _ -> false

let extract_binding_info b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in
  (original_rhs, fn_name, params)

(* TODO truncate, don't fail *)
let count_params fn_name params =
  let count = List.length params in
  if count < 1 || count > 7 then
    params_out_of_range fn_name count

(** We only take parameters within the list, but if it is empty,
    we don't filter at all. *)
let filter_params names params =
  match names with
  | [] -> params
  | _ -> List.filter (fun p -> List.mem p names) params

let run_invocation fn_name params config fn =
  let open TraceConfig in
  let filtered_params = filter_params config.vars params in
  let filtered_param_count = List.length filtered_params in
  let run_fn_name = ident_dot
      (config.module_prefix @ [Names.run_n filtered_param_count]) in
  let final_fn =
    if filtered_param_count = List.length params then fn
    else
      fun_wildcards filtered_param_count (app fn (List.map ident params))
  in
  let invocation = app run_fn_name (List.concat [
      [str fn_name];
      List.map (fun p -> Exp.tuple [str p; [%expr PolyPrint.show]; ident p]) filtered_params;
      [[%expr PolyPrint.show]];
      [final_fn]])
  in
  print_expr invocation;
  invocation

let transform_binding_recursively config b =
  let open TraceConfig in
  let (original_rhs, fn_name, params) = extract_binding_info b in
  count_params fn_name params;
  let nonrec_body =
    let nonrec_params = "self" :: params in
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name "self" in
    let new_body = mapper.expr mapper body in
    fun_with_params nonrec_params new_body
  in
  let new_rhs = fun_with_params params [%expr
      let [%p pat_var (mangle fn_name)] = [%e nonrec_body] in
      let rec aux =
        [%e fun_with_params params
              (run_invocation fn_name params config
                 (app (ident (mangle fn_name)) [ident "aux"]));
        ] in [%e app_variables "aux" params]
    ] in
  { b with pvb_expr = new_rhs }

let transform_binding_nonrecursively config b =
  let open TraceConfig in
  let (original_rhs, fn_name, params) = extract_binding_info b in
  count_params fn_name params;
  let new_rhs =
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name (mangle fn_name) in
    let new_body = mapper.expr mapper body in
    fun_with_params params new_body
  in
  (* let rec is used here when transforming recursive functions.
     Non-recursive functions wouldn't have recursive references,
     so this is safe. *)
  let new_rhs = fun_with_params params [%expr
      let rec [%p pat_var (mangle fn_name)] = [%e new_rhs] in
      [%e run_invocation fn_name params config
            (ident (mangle fn_name))]]
  in
  { b with pvb_expr = new_rhs }

let transform_recursive_binding attrs b =
  let config = TraceConfig.interpret attrs in
  if any (has_attr "tracerec") attrs then
    transform_binding_recursively config b
  else
    transform_binding_nonrecursively config b

let transform_nonrecursive_binding attrs b =
  let config = TraceConfig.interpret attrs in
  transform_binding_nonrecursively config b

let interesting_expr_binding rec_flag attrs binding =
  let has_attr =
    match rec_flag with
    | Nonrecursive ->
      if any (has_attr "tracerec") attrs then
        tracerec_used_on_nonrecursive_binding ()
      else
        any (has_attr "trace") attrs
    | Recursive ->
      any (has_attr "trace") attrs || any (has_attr "tracerec") attrs
  in has_attr && is_function_binding binding

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

let mapper =
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
