
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

type log_config = string

let rec contains_log attr =
  let open Parsetree in
  match attr with
  | { txt = name }, _ when name = "log" -> true
  | _ -> false

let rec get_log_config attr =
  let open Parsetree in
  if List.length attr = 0 then None
  else match List.hd attr with
  | _,
    PStr [{pstr_desc =
                Pstr_eval (
                  {pexp_desc = Pexp_construct ({txt = Lident name}, None)}
                , _)
             }] ->
    Some name
  (* | _, PStr [{pstr_desc = *)
  (*               Pstr_eval ( *)
  (*                 {pexp_desc = *)
  (*                    Pexp_sequence *)
  (*                      ({pexp_desc = Pexp_construct ({txt = Lident name}, None)}, *)
  (*                       {pexp_desc = Pexp_ident {txt = Lident "x"}})}, _) *)
  (*            }] when name = "log" -> failwith "not yet implemented" *)
  | _ -> None

(** Transformations on bindings *)

let transform_recursive_binding b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in
  let mangled = mangle fn_name in

  let config = get_log_config b.pvb_attributes in
  let config_module =
    match config with
    | None -> [Names.runtime; Names.default_log]
    | Some c -> [c]
  in
  let nonrec_params = "self" :: params in
  let nonrec_body =
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name "self" in
    let new_body = mapper.expr mapper body in
    fun_of_params nonrec_params new_body
  in

  let param_count = List.length params in
  if param_count < 1 || param_count > 7 then
    params_out_of_range fn_name param_count
  else
    (* let printer = ident_dot ["Debug"; "no_" ^ string_of_int param_count] in *)
    (* let printer = ident_dot ["Debug"; "no_" ^ string_of_int param_count] in *)
    (* let printer = ident_dot [Names.runtime; Names.default_log; Names.run_n param_count] in *)
    let printer = ident_dot (config_module @ [Names.run_n param_count]) in
    let print_invocation = app printer
        (List.concat [
            [str fn_name];
            List.map (fun p -> Exp.tuple [str p; [%expr PolyPrint.show]; ident p]) params;
            [[%expr PolyPrint.show]];
            [app (ident mangled) [ident "aux"]]
          ])
    in
    (* let print_invocation = app printer *)
    (* ([str fn_name] @ *)
    (* (List.map (fun _ -> [%expr PolyPrint.show]) params) @ *)
    (* [[%expr PolyPrint.show]] @ *)
    (* [app (ident mangled) [ident "aux"]] @ *)
    (* List.map ident params) *)

    (* List.map (fun p -> Exp.tuple [str p; [%expr PolyPrint.show]; ident p]) params) *)
    (* in *)
    let new_rhs = fun_of_params params [%expr
        let [%p pat_var mangled] = [%e nonrec_body] in
        let rec aux =
          [%e fun_of_params params [%expr
                 (* [%e app (ident_dot ["Printf"; "printf"]) *)
                (* ([str fmt] @ List.map to_string params)]; *)
                (* PolyPrint.Default.print1 [%e str fn_name] ([%e str @@ List.hd params], PolyPrint.show, [%e ident @@ List.hd params]); *)
              
                [%e print_invocation];
                (* (); *)

                (* let result = [%e app_variables mangled ("aux" :: params)] in *)
                (* PolyPrint.Default.print_result [%e str fn_name] (PolyPrint.show) result; *)
                (* result *)
              ]] in [%e app_variables "aux" params]
      ] in
    { b with pvb_expr = new_rhs }

let transform_nonrecursive_binding b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in
  let mangled = mangle fn_name in

  let new_rhs = fun_of_params params [%expr
      let [%p pat_var mangled] = [%e original_rhs] in
      let result = [%e app_variables mangled params] in
      PolyPrint.print result;
      result] in
  { b with pvb_expr = new_rhs }

(** Transformations on expressions *)

let transform_expr rec_flag transform expr mapper bindings body =
  let transform_interesting_binding b =
    let interesting = any contains_log expr.pexp_attributes && is_function_binding b in
    if interesting then
      transform b
    else
      { b with pvb_expr = mapper.expr mapper b.pvb_expr }
  in
  let new_bindings = List.map transform_interesting_binding bindings in
  { expr with pexp_desc = Pexp_let (rec_flag, new_bindings, mapper.expr mapper body) }

let transform_let expr mapper bindings body =
  transform_expr Nonrecursive transform_nonrecursive_binding expr mapper bindings body

let transform_letrec expr mapper bindings body =
  transform_expr Recursive transform_recursive_binding expr mapper bindings body

(** Transformations on structure items *)

let transform_str rec_flag transform mapper bindings =
  let transform_interesting_binding b =
    let interesting = any contains_log b.pvb_attributes && is_function_binding b in
    if interesting then
      transform b
    else
      { b with pvb_expr = mapper.expr mapper b.pvb_expr }
  in
  let new_bindings =
    bindings
    |> List.map transform_interesting_binding
    (* |> List.map (fun b -> { b with pvb_expr = mapper.expr mapper b.pvb_expr }) *)
  in
  item @@ Pstr_value (rec_flag, new_bindings)

let transform_str_letrec mapper bindings =
  transform_str Recursive transform_recursive_binding mapper bindings

let transform_str_let mapper bindings =
  transform_str Nonrecursive transform_nonrecursive_binding mapper bindings

let mapper =
  { default_mapper with
    expr =
      begin
        fun mapper expr ->
          match expr with
          | { pexp_desc = Pexp_let (rec_flag, bindings, body) } ->
            begin
              match rec_flag with
              | Recursive -> transform_letrec expr mapper bindings body
              | Nonrecursive -> transform_let expr mapper bindings body
            end
          | x -> default_mapper.expr mapper x;
      end;
    structure_item = fun mapper item ->
      match item with
      | { pstr_desc = Pstr_value (rec_flag, bindings) } ->
        begin
          match rec_flag with
          | Recursive -> transform_str_letrec mapper bindings
          | Nonrecursive -> transform_str_let mapper bindings
        end
      | s -> default_mapper.structure_item mapper s
  }
