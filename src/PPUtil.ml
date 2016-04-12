
open Ast_mapper
open Asttypes
open Longident

module Names = struct
  open Printf
  open Str
      
  let runtime = "PolyPrint"
  let printers = "Printers"
  let default_module = "DefaultTraceConfig"
  let to_string = ["to_string"; "string_of"; "show"]
  let print = "print"
  let debug = "debug"
  let run_n n = "run" ^ string_of_int n
  let call_n n = "call" ^ string_of_int n
  let traced_n n = "Traced" ^ string_of_int n
  let traced_type_n n = "traced" ^ string_of_int n
  let traced_arity name =
    try
      ignore @@ search_forward (regexp "traced\\([0-7]\\)") name 0;
      matched_group 1 name |> int_of_string
    with Not_found ->
      failwith @@ sprintf "%s is not a valid name for a traced function type" name
  let is_traced_type name =
    try
      ignore @@ traced_arity name; true
    with Not_found -> false

  let wrap_n n = "wrap" ^ string_of_int n
  let mangle fn_name = fn_name ^ "_original"
  let self name = name ^ "_self"
  let unself name =
    let length = String.length name in
    if length < 5 then name
    else String.sub name 0 (length - 5)
end

module Errors = struct
  let tracerec_used_on_nonrecursive_binding () =
    failwith @@ Printf.sprintf "[@tracerec] cannot be used on a non-recursive binding"

  let warn_too_many_params =
    Printf.printf "Warning: %s was given %d arguments but this library only supports up to 7.\n"
end

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

let rec range l u =
  if l = u then []
  else l :: range (l + 1) u

let otherwise default e =
  match e with
  | None -> default
  | Some x -> x

let string_of_list pr xs =
  let rec aux xs =
    match xs with
    | [] -> ""
    | [x] -> pr x
    | y :: ys -> pr y ^ "; " ^ aux ys
  in "[" ^ aux xs ^ "]"

let replace needle haystack =
  Str.global_replace (Str.regexp needle) haystack

let starts_with prefix s =
  try
    ignore (Str.search_forward (Str.regexp prefix) s 0);
    true
  with Not_found -> false

let truncate n s =
  if String.length s <= n then s
  else String.sub s 0 n ^ "..."

let tap msg action x =
  print_string @@ msg ^ ": ";
  action x;
  x

let taps msg x =
  print_endline @@ msg ^ ": ";
  x

let dummy_loc = {
  Location.loc_start = Lexing.dummy_pos;
  Location.loc_end = Lexing.dummy_pos;
  Location.loc_ghost = true
}

module Untyped = struct

  open Ast_helper
  open Parsetree

  let show_structure = Pprintast.string_of_structure

  let print_structure e =
    show_structure e |> print_endline

  let show_expr = Pprintast.string_of_expression

  let print_expr e =
    show_expr e |> print_endline

  let app ?(loc=dummy_loc) f args =
    Exp.apply ~loc f (List.map (fun a -> "", a) args)

  let ident ?(loc=dummy_loc) s =
    Exp.ident ~loc { txt = Lident s; loc }

  let str ?(loc=dummy_loc) s =
    Exp.constant ~loc (Const_string (s, None))

  let qualified_ident ?(loc=dummy_loc) ss =
    match ss with
    | [] -> failwith "empty dotted identifier"
    | [s] -> ident s
    | s :: ss ->
        let res = List.fold_left (fun t c -> Ldot (t, c)) (Lident s) ss in
        Exp.ident ~loc { txt = res; loc }

  let is_function_binding b =
    match b with
    | { pvb_expr = { pexp_desc = Pexp_fun _ } } -> true
    | _ -> false

  let get_fn_name pat =
    match pat with
    | { ppat_desc = Ppat_var { txt = fn_name }; _ } -> fn_name
    | _ -> failwith "not a function pattern"

  (** Recurses down a curried lambda to get the body *)
  let rec get_fn_body ?(loc=dummy_loc) pexp =
    match pexp with
    | { pexp_desc = Pexp_fun (_, _, _, b); pexp_loc } -> get_fn_body ~loc:pexp_loc b
    | _ -> pexp, loc

  let pat_any = {
    ppat_desc = Ppat_any;
    ppat_loc = dummy_loc;
    ppat_attributes = []
  }

  let pat_var name = {
    ppat_desc = Ppat_var { txt = name; loc = dummy_loc };
    ppat_loc = dummy_loc;
    ppat_attributes = []
  }

  let pat_unit = {
    ppat_desc = Ppat_construct ({ txt = Lident "()"; loc = dummy_loc }, None);
    ppat_loc = dummy_loc;
    ppat_attributes = []
  }

  type param = Param of string | Unit

  let show_param p =
    match p with
    | Unit -> "()"
    | Param x -> x

  let rec collect_params f =
    match f with
    | { pexp_desc = Pexp_fun (_, _, { ppat_desc = desc; _ }, rest ) } ->
        begin match desc with
        | Ppat_var { txt = param; _ }
        | Ppat_constraint ({ ppat_desc = Ppat_var { txt = param; _ } }, _) ->
            Param param :: collect_params rest
        | Ppat_construct ({txt = Lident "()"}, None) ->
            Unit :: collect_params rest
        | _ -> []
        end
    | _ -> []

  let param_to_expr ?(loc=dummy_loc) p =
    match p with
    | Unit -> Exp.construct ~loc ({ txt = Lident "()"; loc }) None
    | Param name -> Exp.ident ~loc { txt = Lident name; loc }

  let param_to_arg ?(loc=dummy_loc) p =
    "", param_to_expr ~loc p

  let app_variables ?(loc=dummy_loc) f args =
    Exp.apply ~loc (ident f) (List.map param_to_arg args)

  let rec fun_with_params ?(loc=dummy_loc) params body =
    match params with
    | [] -> body
    | p :: ps ->
        let p' =
          match p with
          | Param x -> pat_var x
          | Unit -> pat_unit
        in Exp.fun_ ~loc "" None p' (fun_with_params ~loc ps body)

  let rec eta_abstract ?(loc=dummy_loc) n exp =
    let make_param s = Param ("_" ^ string_of_int s) in
    let params = range 0 n |> List.map make_param in
    fun_with_params ~loc params
      (Exp.apply ~loc exp (List.rev params |> List.map param_to_arg))

  (* Returns a lambda with n wildcard parameters, e.g. fun _ _ -> body *)
  let rec fun_wildcards ?(loc=dummy_loc) n body =
    match n with
    | 0 -> body
    | _ -> Exp.fun_ ~loc "" None pat_any (fun_wildcards ~loc (n - 1) body)

  let rec longident_to_list li =
    match li with
    | Lident name -> [name]
    | Ldot (a, b) -> longident_to_list a @ [b]
    | Lapply _ -> failwith "longident apply cannot be converted"

  let item desc = {
    pstr_desc = desc;
    pstr_loc = dummy_loc;
  }

  let app_mapper find replace =
    { default_mapper with
      expr = fun mapper expr ->
        match expr with
        | { pexp_desc =
              Pexp_apply
                ({ pexp_desc = Pexp_ident { txt = Lident fn_name; loc } }, args) }
          when fn_name = find ->
            Exp.apply ~loc (Exp.ident ~loc { txt = Lident replace; loc }) args
        | { pexp_desc =
              Pexp_apply
                ({ pexp_desc = Pexp_ident { txt = Ldot (initial, fn_name); loc } }, args) }
          when fn_name = find ->
            Exp.apply ~loc (Exp.ident ~loc { txt = Ldot (initial, replace); loc }) args
        | _ -> default_mapper.expr mapper expr
    }

  let has_attr name attr =
    match attr with
    | { txt = n }, _ when n = name -> true
    | _ -> false

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
        txt = Lident name;
        loc = dummy_loc
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

  let show_type ty =
    Printtyp.reset ();
    Printtyp.mark_loops ty;
    Format.asprintf "%a" Printtyp.type_expr ty

  let print_type ty =
    ty |> show_type |> print_endline

  let show_expr e =
    Typpx.Untypeast.untype_expression e
    |> Pprintast.string_of_expression
    |> replace " +" " "

  let print_expr e =
    print_endline @@ show_expr e

  let show_structure e =
    Typpx.Untypeast.untype_structure e
    |> Untyped.show_structure

  let print_structure s =
    print_endline @@ show_structure s

end

