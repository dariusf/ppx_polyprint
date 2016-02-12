
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

let logrec_used_on_nonrecursive_binding () =
  failwith @@ Printf.sprintf "[@logrec] cannot be used on a non-recursive binding"

type log_config = string

let has_attr name attr =
  match attr with
  | { txt = n }, _ when n = name -> true
  | _ -> false

let rec get_log_config attr =
  match attr with
  | [] -> None
  | _ ->
    begin match List.hd attr with
      | _, PStr [{pstr_desc = Pstr_eval
                      ({pexp_desc = Pexp_construct ({txt = Lident name}, None)} , _)}] ->
        Some name
      (* | _, PStr [{pstr_desc = *)
      (*               Pstr_eval ( *)
      (*                 {pexp_desc = *)
      (*                    Pexp_sequence *)
      (*                      ({pexp_desc = Pexp_construct ({txt = Lident name}, None)}, *)
      (*                       {pexp_desc = Pexp_ident {txt = Lident "x"}})}, _) *)
      (*            }] when name = "log" -> failwith "not yet implemented" *)
      | _ -> None
    end

let extract_binding_info b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in
  (original_rhs, fn_name, params)

let check_config attrs =
  let config = get_log_config attrs in
  let module_prefix =
    match config with
    | None -> [Names.runtime; Names.default_module]
    | Some c -> [c]
  in
  module_prefix

let count_params fn_name params =
  let count = List.length params in
  if count < 1 || count > 7 then
    params_out_of_range fn_name count
  else count

let run_invocation fn_name params param_count module_prefix fn =
  let run_fn = ident_dot (module_prefix @ [Names.run_n param_count]) in
  let invocation = app run_fn (List.concat [
      [str fn_name];
      List.map (fun p -> Exp.tuple [str p; [%expr PolyPrint.show]; ident p]) params;
      [[%expr PolyPrint.show]];
      [fn]])
  in invocation

let transform_binding_recursively attrs b =
  let (original_rhs, fn_name, params) = extract_binding_info b in
  let module_prefix = check_config attrs in
  let param_count = count_params fn_name params in
  let nonrec_body =
    let nonrec_params = "self" :: params in
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name "self" in
    let new_body = mapper.expr mapper body in
    fun_of_params nonrec_params new_body
  in
  let new_rhs = fun_of_params params [%expr
      let [%p pat_var (mangle fn_name)] = [%e nonrec_body] in
      let rec aux =
        [%e fun_of_params params
              (run_invocation fn_name params param_count module_prefix
                 (app (ident (mangle fn_name)) [ident "aux"]));
        ] in [%e app_variables "aux" params]
    ] in
  { b with pvb_expr = new_rhs }

(* rather than take attrs, these should take configs *)
let transform_binding_nonrecursively attrs b =
  let (original_rhs, fn_name, params) = extract_binding_info b in
  let module_prefix = check_config attrs in
  let param_count = count_params fn_name params in
  let new_rhs =
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name (mangle fn_name) in
    let new_body = mapper.expr mapper body in
    fun_of_params params new_body
  in
  (* let rec is used here when transforming recursive functions.
     Non-recursive functions wouldn't have recursive references,
     so this is safe. *)
  let new_rhs = fun_of_params params [%expr
      let rec [%p pat_var (mangle fn_name)] = [%e new_rhs] in
      [%e run_invocation fn_name params param_count module_prefix
            (ident (mangle fn_name))]]
  in
  { b with pvb_expr = new_rhs }

let transform_recursive_binding attrs b =
  if any (has_attr "logrec") attrs then
    transform_binding_recursively attrs b
  else
    transform_binding_nonrecursively attrs b

let transform_nonrecursive_binding = transform_binding_nonrecursively

let interesting_expr_binding rec_flag attrs binding =
  let has_attr =
    match rec_flag with
    | Nonrecursive ->
      if any (has_attr "logrec") attrs then
        logrec_used_on_nonrecursive_binding ()
      else
        any (has_attr "log") attrs
    | Recursive ->
      any (has_attr "log") attrs || any (has_attr "logrec") attrs
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
