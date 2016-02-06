
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

open PolyPrintUtil

let string_of_list pr xs =
  let rec aux xs =
    match xs with
    | [] -> ""
    | [x] -> pr x
    | y :: ys -> pr y ^ "; " ^ aux ys
  in "[" ^ aux xs ^ "]"

let print_attr_names_in_bindings bs =
  let names = List.concat @@ List.map (fun b -> b.pvb_attributes) bs in
  print_endline @@ string_of_list (fun x -> x) @@ List.map (fun ({txt=name}, _) -> name) names

let rec contains_log attr =
  let open Parsetree in
  match attr with
  | { txt = name }, _ when name = "log" -> true
  | _ -> false

let attrs_of_bindings bs =
  List.concat @@ List.map (fun b -> b.pvb_attributes) bs

let rec collect_params f =
  match f with
  | { pexp_desc = Pexp_fun (_, _, {
      ppat_desc = Ppat_var { txt = param; _ }; _ }, rest ) } ->
    param :: collect_params rest
  | _ -> []

let rec fun_of_params params body =
  match params with
  | [] -> body
  | p :: ps -> Exp.fun_ "" None (pat_var p) (fun_of_params ps body)

let get_fn_name pat =
  match pat with
  | { ppat_desc = Ppat_var { txt = fn_name }; _ } -> fn_name
  | _ -> failwith "not a function pattern"

let app_variables f args =
  let ident s = Exp.ident { txt = Lident s; loc = dummy_loc } in
  Exp.apply (ident f) (List.map (fun a -> "", ident a) args)

let app f args =
  Exp.apply f (List.map (fun a -> "", a) args)

let ident s =
  Exp.ident { txt = Lident s; loc = dummy_loc }

let str s =
  Exp.constant (Const_string (s, None))

let to_string param = [%expr PolyPrint.to_string [%e ident param]]

let param_fmt_string fn_name params =
  params
  |> List.map (fun n -> n ^ ": %s")
  |> String.concat ", "
  |> (fun s -> fn_name ^ " = " ^ s ^ "\n")

let ident_dot ss =
  match ss with
  | [] -> failwith "empty dotted identifier"
  | [s] -> ident s
  | s :: ss ->
    let res = List.fold_left (fun t c -> Ldot (t, c)) (Lident s) ss in
    Exp.ident { txt = res; loc = dummy_loc }

let item desc =
  {
    pstr_desc = desc;
    pstr_loc = dummy_loc;
  }

let is_function_binding b =
  match b with
  | { pvb_expr = { pexp_desc = Pexp_fun _ } } -> true
  | _ -> false

let rec get_fn_body pexp =
  match pexp with
  | { pexp_desc = Pexp_fun (_, _, _, b) } -> get_fn_body b
  | _ -> pexp

let all f xs =
  List.fold_left (fun t c -> t && f c) true xs

let any f xs =
  List.fold_left (fun t c -> t || f c) false xs

let mangle fn_name =
  fn_name ^ "_original"

(** Transformations on bindings *)

let transform_recursive_binding b =
  let { pvb_pat = original_lhs; pvb_expr = original_rhs } = b in
  let fn_name = get_fn_name original_lhs in
  let params = collect_params original_rhs in
  let mangled = mangle fn_name in

  let nonrec_params = "self" :: params in
  let nonrec_body =
    let body = get_fn_body original_rhs in
    let mapper = app_mapper fn_name "self" in
    let new_body = mapper.expr mapper body in
    fun_of_params nonrec_params new_body
  in
  let fmt = param_fmt_string fn_name params in
  let new_rhs = fun_of_params params [%expr
      let [%p pat_var mangled] = [%e nonrec_body] in
      let rec aux =
        [%e fun_of_params params [%expr
              [%e app (ident_dot ["Printf"; "printf"])
                    ([str fmt] @ List.map to_string params)];
              let result = [%e app_variables mangled ("aux" :: params)] in
              (PolyPrint.print result; result)
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
