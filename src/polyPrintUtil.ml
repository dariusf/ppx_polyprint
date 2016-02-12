
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

module Names = struct
  let runtime = "PolyPrint"
  let printers = "Printers"
  let default_log = "Default"
  let to_string = ["to_string"; "string_of"; "show"]
  let print = "print"
  let run_n n = "run" ^ string_of_int n
end

let dummy_loc =
  {
    Location.loc_start = Lexing.dummy_pos;
    Location.loc_end = Lexing.dummy_pos;
    Location.loc_ghost = true
  }

let all f xs =
  List.fold_left (fun t c -> t && f c) true xs

let any f xs =
  List.fold_left (fun t c -> t || f c) false xs

module Untyped = struct

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

  let ident_dot ss =
    match ss with
    | [] -> failwith "empty dotted identifier"
    | [s] -> ident s
    | s :: ss ->
      let res = List.fold_left (fun t c -> Ldot (t, c)) (Lident s) ss in
      Exp.ident { txt = res; loc = dummy_loc }

  let is_function_binding b =
    match b with
    | { pvb_expr = { pexp_desc = Pexp_fun _ } } -> true
    | _ -> false

  let rec get_fn_body pexp =
    match pexp with
    | { pexp_desc = Pexp_fun (_, _, _, b) } -> get_fn_body b
    | _ -> pexp

  let app_mapper find replace =
    { default_mapper with
      expr = fun mapper expr ->
        match expr with
        | { pexp_desc =
              Pexp_apply
                ({ pexp_desc = Pexp_ident { txt = Lident fn_name; loc } }, args) }
          when fn_name = find ->
          Exp.apply (Exp.ident { txt=Lident replace; loc }) args
        | _ -> default_mapper.expr mapper expr
    }

  let pat_var name =
    { ppat_desc = Ppat_var { txt = name; loc = dummy_loc };
      ppat_loc = dummy_loc;
      ppat_attributes = []
    }

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

  (* let binding pat expr = *)
  (*   { pvb_pat = pat; *)
  (*     pvb_expr = expr; *)
  (*     pvb_loc = dummy_loc; *)
  (*     pvb_attributes = [] *)
  (*   } *)

  let item desc =
    {
      pstr_desc = desc;
      pstr_loc = dummy_loc;
    }

end

module Typed = struct
end

