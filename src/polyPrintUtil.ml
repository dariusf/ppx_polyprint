
open Ast_mapper
open Ast_helper
open Asttypes
open Longident

module Names = struct
  let runtime = "PolyPrint"
  let printers = "Printers"
  let default_module = "DefaultTraceConfig"
  let to_string = ["to_string"; "string_of"; "show"]
  let print = "print"
  let debug = "debug"
  let run_n n = "run" ^ string_of_int n
  let call_n n = "call" ^ string_of_int n
  let mangle fn_name = fn_name ^ "_original"
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

let push x xs =
  xs := x :: !xs

let clear xs =
  xs := []

let clamp l h x =
  max l (min h x)

let otherwise default e =
  match e with
  | None -> default
  | Some x -> x

module Untyped = struct

  open Parsetree

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

  let pat_any () = {
    ppat_desc = Ppat_any;
    ppat_loc = dummy_loc;
    ppat_attributes = []
  }

  let pat_var name = {
    ppat_desc = Ppat_var { txt = name; loc = dummy_loc };
    ppat_loc = dummy_loc;
    ppat_attributes = []
  }

  let rec collect_params f =
    match f with
    | { pexp_desc = Pexp_fun (_, _, {
        ppat_desc = Ppat_var { txt = param; _ }; _ }, rest ) } ->
      param :: collect_params rest
    | _ -> []

  let rec fun_with_params params body =
    match params with
    | [] -> body
    | p :: ps -> Exp.fun_ "" None (pat_var p) (fun_with_params ps body)

  let rec fun_wildcards n body =
    match n with
    | 0 -> body
    | _ -> Exp.fun_ "" None (pat_any ()) (fun_wildcards (n - 1) body)

  let rec longident_to_list li =
    match li with
    | Lident name -> [name]
    | Ldot (a, b) -> longident_to_list a @ [b]
    | Lapply _ -> failwith "longident apply cannot be converted"
    
  let item desc = {
    pstr_desc = desc;
    pstr_loc = dummy_loc;
  }

  let print_expr e =
    print_endline @@ Pprintast.string_of_expression e

end

module Typed = struct

  open Typedtree

  let pident name = Path.Pident { Ident.name = name; stamp = 0; flags = 0 }

  let pdot t name = Path.Pdot (t, name, 0)

  (* let failure loc = *)
  (*   raise *)
  (*     (Location.Error *)
  (*        (Location.error ~loc "[%pretty] accepts a string, e.g. [%pretty \"USER\"]")) *)

  let dummy_value_desc =
    Types.{
      val_type = { level = 0; id = 0; desc = Tnil };
      val_kind = Val_reg;
      val_loc = dummy_loc;
      val_attributes = [];
    }

  let dummy_type =
    Types.{
      desc = Tvar (Some "");
      level = 0;
      id = 0;
    }

  let expr_from_desc ?(loc=dummy_loc) desc =
    {
      exp_desc = desc;
      exp_loc = loc;
      exp_extra = [];
      exp_type = dummy_type;
      exp_env = Env.empty;
      exp_attributes = [];
    }

  let dummy_args args =
    List.map (fun a -> "", Some a, Required) args

  let string_desc s =
    Texp_constant (Const_string (s, None))

  let tstr s =
    expr_from_desc (string_desc s)

  let print_ident { Ident.stamp=stamp; Ident.name=name; Ident.flags=flags } =
    (* "{stamp=" ^ string_of_int stamp ^ " name=" ^ name ^ " flags=" ^ string_of_int flags ^ "}" *)
    name

  (* Printtyp.string_of_path  *)

  let to_path p =
    let rec aux p =
      match p with
      | [] -> assert false
      | [x] -> pident x
      | x :: xs -> pdot (aux xs) x
    in aux (List.rev p)

  let to_longident p =
    let rec aux p =
      match p with
      | [] -> assert false
      | [x] -> Lident x
      | x :: xs -> Ldot (aux xs, x)
    in aux (List.rev p)

  let tident name =
    expr_from_desc @@ Texp_ident (
      pident name, {
        txt=Lident name;
        loc=dummy_loc
      },
      dummy_value_desc)

  let tident_ qname =
    expr_from_desc @@ Texp_ident (
      to_path qname, {
        txt=to_longident qname;
        loc=dummy_loc
      },
      dummy_value_desc)

  let qualified name =
    tident_ [Names.runtime; Names.printers; name]

  let rec path_to_longident p =
    let open Path in
    match p with
    | Pident { Ident.name; _ } -> Lident name
    | Pdot (t, name, _) -> Ldot (path_to_longident t, name)
    | Papply _ -> assert false

  let tident_with_path prefix name =
    let pd = pdot prefix name in
    expr_from_desc @@ Texp_ident (
      pd, {
        txt = path_to_longident pd;
        loc =dummy_loc
      },
      dummy_value_desc)

  let appl_desc (name:string) args =
    Texp_apply (tident name, args)

  let tapp exp args =
    match args with
    | [] -> exp
    | _ ->
      expr_from_desc @@ Texp_apply (exp, dummy_args args)

  let args_to_exprs args =
    let remove_opt a =
      match a with
      | Some b -> b
      | None -> failwith "expected a to have a value"
    in
    args |> List.map (fun (_, a, _) -> a) |> List.map remove_opt

  let print_type ty =
    Printtyp.reset ();
    Printtyp.mark_loops ty;
    Format.asprintf "%a" Printtyp.type_expr ty

  let string_of_expr e =
    Pprintast.string_of_expression @@
    Typpx.Untypeast.untype_expression e

  let print_expr e =
    print_endline @@ string_of_expr e

end

