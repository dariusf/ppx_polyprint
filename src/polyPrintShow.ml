
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Typedtree

open PolyPrintUtil

module Names = struct
  let runtime = "PolyPrint"
  let printers = "Printers"
  let to_string = ["to_string"; "string_of"; "show"]
  let print = "print"
end

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

type path = string list

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

let print_type ty =
  Printtyp.reset ();
  Printtyp.mark_loops ty;
  Format.asprintf "%a" Printtyp.type_expr ty

let remove_opt a =
  match a with
  | Some b -> b
  | None -> failwith "expected a to have a value"

let args_to_exprs args =
  args |> List.map (fun (_, a, _) -> a) |> List.map remove_opt

let constant_printer which args =
  let name =
    match which with
    | "string" -> "id"
    | "int" -> "string_of_int"
    | "int32" -> "string_of_int32"
    | "int64" -> "string_of_int64"
    | "nativeint" -> "string_of_nativeint"
    | "bool" -> "string_of_bool"
    | "char" -> "string_of_char"
    | "float" -> "string_of_float"
    | "exn" -> "string_of_exn"
    | _ -> failwith @@ "printer for constant type " ^ which ^ " not implemented"
  in
  tapp (qualified name) args

let rec build_printer : Types.type_expr -> expression list -> expression =
  fun ty args ->
    let open Types in
    (* print_endline @@ print_type ty; *)
    match ty.desc with
    | Tconstr (path_t, texprs, abbrev) ->
      begin
        let name =
          let open Path in
          match path_t with
          | Pident { Ident.name; _ } ->
            begin match name with
              | ("int" | "string" | "bool" | "char" | "float" | "exn" |
                 "int32" | "int64" | "nativeint") as name ->
                constant_printer name args
              | "list" ->
                tapp (tapp (qualified "string_of_list")
                        (List.map (fun t -> build_printer t []) texprs)) args
              | "option" ->
                tapp (tapp (qualified "string_of_option")
                        (List.map (fun t -> build_printer t []) texprs)) args
              | "ref" ->
                tapp (tapp (qualified "string_of_ref")
                        (List.map (fun t -> build_printer t []) texprs)) args
              | t ->
                tapp (tapp (tident ("show_" ^ t))
                        (List.map (fun t -> build_printer t []) texprs)) args
            end
          | Pdot (Pident { Ident.name = prefix; _ }, name, _) when prefix = "Pervasives"->
            build_printer { ty with desc = Tconstr (pident name, texprs, abbrev) } args
          | Pdot (prefix, t, _) ->
            tapp (tapp (tident_with_path prefix ("show_" ^ t))
                    (List.map (fun t -> build_printer t []) texprs)) args
          | _ -> failwith "Papply not yet implemented"
        in name
      end

    | Tarrow _ -> tapp (qualified "string_of_function") args
    | Ttuple tys ->
      let length = List.length tys in
      assert (length >= 2 && length <= 7);
      let suffix = if length = 2 then "" else string_of_int length in
      let name = "string_of_tuple" ^ suffix in
      tapp (tapp (qualified name) (List.map (fun t -> build_printer t []) tys)) args
    | Tlink t -> build_printer t args
    | Tvar v -> (* what is v? monomorphic type from value restriction? *)
      begin
        match v with
        | Some v ->
          tapp (tapp (qualified "string_of_tvar") [tstr v]) args
        | None ->
          tapp (tapp (qualified "string_of_tvar") [tstr ""]) args
      end
    | Tobject (t, _) ->
      tapp (tapp (qualified "message") [tstr (print_type t)]) args
    | Tfield _ -> failwith "not implemented field"
    | Tnil -> failwith "not implemented nil"
    | Tsubst _ -> failwith "not implemented subst"
    | Tvariant _ -> failwith "not implemented variant"
    | Tunivar _ -> failwith "not implemented univar"
    | Tpoly _ -> failwith "not implemented poly"
    | Tpackage _ -> failwith "not implemented pkg"

let transform_printer e args =
  let open Types in
  begin match args with
    | [] ->
      (* printers must be used in a higher-order context *)
      begin match e.exp_desc with
        | Texp_ident (_, _, { val_type = ty }) ->

          begin match ty.desc with
            | Tarrow (_, a, b, _) ->
              { e with exp_desc = (build_printer a []).exp_desc }
            | _ -> assert false
          end

        | _ -> assert false (* better error handling required *)
      end

    | ["", Some ({ exp_type = ty } as _a), Required] ->
      (* printers are called with a single argument *)
      { e with exp_desc =
                 (build_printer ty (args_to_exprs args)).exp_desc }
    | _ ->
      (* better error handling required *)
      failwith (Printf.sprintf "no function in the API has %d arguments" (List.length args))
  end

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let enter_expression e =
    let open Path in
    match e.exp_desc with
    | Texp_apply
        ({exp_desc =
            Texp_ident (Pdot (Pident {Ident.name = mod_name}, fn, _), _loc, _) }, args)
      when mod_name = Names.runtime ->

      if List.mem fn Names.to_string then
        transform_printer e args
      else if fn = Names.print then
        tapp (tident "print_endline") [transform_printer e args]
      else e

    | Texp_ident (Pdot (Pident {Ident.name = mod_name}, fn, _), _loc, _)
      when mod_name = Names.runtime ->

      if List.mem fn Names.to_string then
        transform_printer e []
      else if fn = Names.print then
        tapp (tident "print_endline") [transform_printer e []]
      else e

    | _ -> e

  let leave_expression e = e
end

(* TODO factor out *)
let pat_var name =
  let open Parsetree in
  { ppat_desc = Ppat_var { txt = name; loc = dummy_loc };
    ppat_loc = dummy_loc;
    ppat_attributes = [] }

let app f args =
  Exp.apply f (List.map (fun a -> "", a) args)

let ident s =
  Exp.ident { txt = Lident s; loc = dummy_loc }

(** Eta-expand uses of library functions in identifier position only *)
let eta_expansion_mapper =
  let open Parsetree in
  { default_mapper with
    expr = begin
      fun mapper expr ->
      match expr.pexp_desc with
      | Pexp_apply
          ({ pexp_desc =
               Pexp_ident ({ txt = Ldot (Lident mod_name, _) }) } as f, args)
        when mod_name = Names.runtime ->

        { expr with
          pexp_desc = Pexp_apply
              (f, List.map (fun (l, a) -> l, mapper.expr mapper a) args) }

      | Pexp_ident ({ txt = Ldot (Lident mod_name, _) })
        when mod_name = Names.runtime ->

        (* TODO try to replace pat_var and these patterns with metaquot *)
        let res =
        { expr with pexp_desc = Pexp_fun ("", None, pat_var "x", app expr [ident "x"]) }

        in print_endline @@ Pprintast.string_of_expression res; res
      | _ -> default_mapper.expr mapper expr
    end;
    structure_item = fun mapper item ->
      match item with
      | { pstr_desc = Pstr_value (rec_flag, bindings) } ->
        (* Recurse into nested subexpressions *)
        let new_bindings = List.map
            (fun vb -> { vb with pvb_expr = mapper.expr mapper vb.pvb_expr })
            bindings in
        { item with pstr_desc = Pstr_value (rec_flag, new_bindings) }
      | s -> default_mapper.structure_item mapper s
  }

