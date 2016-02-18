
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Typedtree

open PolyPrintUtil
open PolyPrintUtil.Typed

let constant_pp which args =
    match which with
    | "string" -> tapp (tident_ ["Format"; "pp_print_string"]) args
    | "int" -> tapp (tident_ ["Format"; "pp_print_int"]) args
    | "bool" -> tapp (tident_ ["Format"; "pp_print_bool"]) args
    | "char" -> tapp (tident_ ["Format"; "pp_print_char"]) args
    | "float" -> tapp (tident_ ["Format"; "pp_print_float"]) args
    (* | "int32" -> "string_of_int32" *)
    (* | "int64" -> "string_of_int64" *)
    (* | "nativeint" -> "string_of_nativeint" *)
    (* | "exn" -> "string_of_exn" *)
    | _ -> failwith @@ "printer for constant type " ^ which ^ " not implemented"
  (* in *)
  (* tapp (qualified name) args *)

let rec build_pp : Types.type_expr -> expression list -> expression =
  fun ty args ->
    let open Types in
    match ty.desc with
    | Tconstr (path_t, texprs, abbrev) ->
      begin
        let name =
          let open Path in
          match path_t with
          | Pident { Ident.name; _ } ->
            begin match name with
              | ("int" | "string" | "bool" | "char" | "float") as name ->
              (* | "exn" | "int32" | "int64" | "nativeint") *)
                constant_pp name args
              | "list" ->
                tapp (tapp (qualified "pp_list")
                        (List.map (fun t -> build_pp t []) texprs)) args
              | "option" ->
                tapp (tapp (qualified "pp_option")
                        (List.map (fun t -> build_pp t []) texprs)) args
              | "ref" ->
                tapp (tapp (qualified "pp_ref")
                        (List.map (fun t -> build_pp t []) texprs)) args
              | t ->
                tapp (tapp (tident ("pp_" ^ t))
                        (List.map (fun t -> build_pp t []) texprs)) args
            end
          | Pdot (Pident { Ident.name = prefix; _ }, name, _) when prefix = "Pervasives"->
            build_pp { ty with desc = Tconstr (pident name, texprs, abbrev) } args
          | Pdot (prefix, t, _) ->
            tapp (tapp (tident_with_path prefix ("pp_" ^ t))
                    (List.map (fun t -> build_pp t []) texprs)) args
          | _ -> failwith "Papply not yet implemented"
        in name
      end
    | Tarrow _ ->
      tapp (qualified "pp_function") args
    | Ttuple tys ->
      let length = List.length tys in
      assert (length >= 2 && length <= 7);
      let suffix = if length = 2 then "" else string_of_int length in
      let name = "pp_tuple" ^ suffix in
      tapp (tapp (qualified name) (List.map (fun t -> build_pp t []) tys)) args
    | Tlink t -> build_pp t args
    | Tvar v -> (* what is v? monomorphic type from value restriction? *)
      begin
        match v with
        | Some v ->
          tapp (tapp (qualified "pp_tvar") [tstr v]) args
        | None ->
          tapp (tapp (qualified "pp_tvar") [tstr ""]) args
      end
    | Tobject (t, _) ->
      tapp (tapp (qualified "pp_misc") [tstr (print_type t)]) args
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

      let printer =
      tapp (tident_ ["Format"; "asprintf"]) [tstr "%a"; build_pp ty []] in
          begin match ty.desc with
            | Tarrow (_, a, b, _) ->
              { e with exp_desc = printer.exp_desc }
            | _ -> assert false
          end

        | _ -> assert false (* better error handling required *)
      end

    | ["", Some ({ exp_type = ty } as _a), Required] ->
      (* printers are called with a single argument *)
      let printer =
      tapp (tident_ ["Format"; "asprintf"]) ([tstr "%a";
                                            build_pp ty []] @ (args_to_exprs args)) in
      { e with exp_desc = printer.exp_desc }
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
        { expr with pexp_desc = Pexp_fun ("", None, pat_var "x", app expr [ident "x"]) }

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

