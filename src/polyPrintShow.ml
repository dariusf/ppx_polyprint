
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Typedtree

open PolyPrintUtil
open PolyPrintUtil.Typed

let constant_pp primitive =
  match primitive with
  | "string" ->  tident_ ["Format"; "pp_print_string"]
  | "int" ->  tident_ ["Format"; "pp_print_int"]
  | "bool" ->  tident_ ["Format"; "pp_print_bool"]
  | "char" ->  tident_ ["Format"; "pp_print_char"]
  | "float" ->  tident_ ["Format"; "pp_print_float"]
  | "int32" ->  tapp (qualified "pp_int32") []
  | "int64" ->  tapp (qualified "pp_int64") []
  | "nativeint" ->  tapp (qualified "pp_nativeint") []
  | "exn" ->  tapp (qualified "pp_exc") []
  | "unit" ->  tapp (qualified "pp_unit") []
  | _ -> failwith @@ "printer for primitive type " ^ primitive ^ " not implemented"

(** Given the type and value of a function argument, reconstruct the printer for it.*)
let rec build_pp : Types.type_expr -> expression option -> expression =
  fun ty arg ->
    let open Types in
    match ty.desc with
    | Tconstr (path_t, texprs, abbrev) ->
      begin
        let name =
          let open Path in
          match path_t with
          | Pident { Ident.name; _ } ->
            begin match name with
              | ("int" | "string" | "bool" | "char" | "float" | "unit" |
                 "exn" | "int32" | "int64" | "nativeint") as name ->
                constant_pp name
              | ("list" | "option" | "ref") ->
                tapp (qualified ("pp_" ^ name))
                  (List.map (fun t -> build_pp t None) texprs)
              | t ->
                tapp (tident ("pp_" ^ t))
                  (List.map (fun t -> build_pp t None) texprs)
            end
          | Pdot (Pident { Ident.name = prefix; _ }, name, _) when prefix = "Pervasives"->
            build_pp { ty with desc = Tconstr (pident name, texprs, abbrev) } arg
          | Pdot (prefix, t, _) ->
            tapp (tident_with_path prefix ("pp_" ^ t))
              (List.map (fun t -> build_pp t None) texprs)
          | _ -> failwith "Papply not yet implemented"
        in name
      end
    | Tarrow _ ->
      begin match arg with
        | None -> qualified "pp_function"
        | Some a ->
          let repr = truncate 20 (show_expr a) ^ " : " ^ show_type ty in
          tapp (qualified "pp_function_rep") [tstr repr]
      end
    | Ttuple tys ->
      let length = List.length tys in
      assert (length >= 2 && length <= 7);
      let suffix = if length = 2 then "" else string_of_int length in
      let name = "pp_tuple" ^ suffix in
      tapp (qualified name) (List.map (fun t -> build_pp t None) tys)
    | Tlink t -> build_pp t arg
    | Tvar v -> (* what is v? monomorphic type from value restriction? *)
      begin
        match v with
        | Some v ->
          tapp (qualified "pp_tvar") [tstr v]
        | None ->
          tapp (qualified "pp_tvar") [tstr ""]
      end
    | Tobject (t, _) ->
      tapp (qualified "pp_misc") [tstr (show_type t)]
    | Tfield _ -> failwith "not implemented field"
    | Tnil -> failwith "not implemented nil"
    | Tsubst _ -> failwith "not implemented subst"
    | Tvariant _ -> failwith "not implemented variant"
    | Tunivar _ -> failwith "not implemented univar"
    | Tpoly _ -> failwith "not implemented poly"
    | Tpackage _ -> failwith "not implemented pkg"

let transform_printer e args =
  let open Types in
  let arg_count = List.length args in
  let arg_exprs = args_to_exprs args in
  begin match arg_exprs with
    | [] ->
      (* printers must be used in a higher-order context *)
      begin match e.exp_desc with
        | Texp_ident (_, _, { val_type = ty }) ->

          let printer =
            tapp (tident_ ["Format"; "asprintf"]) [tstr "%a"; build_pp ty None] in
          begin match ty.desc with
            | Tarrow (_, a, b, _) ->
              { e with exp_desc = printer.exp_desc }
            | _ -> assert false
          end

        | _ -> assert false (* better error handling required *)
      end

    | [{ exp_type = ty } as arg] ->
      (* printers are called with a single argument *)
      let printer =
        tapp (tident_ ["Format"; "asprintf"]) ([tstr "%a";
                                                build_pp ty (Some arg)] @ [arg]) in
      { e with exp_desc = printer.exp_desc }
    | _ ->
      (* better error handling required *)
      failwith (Printf.sprintf "no function in the API has %d arguments" arg_count)
  end

module TypedTransform : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let rec implementations name fn args =
    if List.mem name Names.to_string then
      transform_printer fn args
    else if name = Names.print then
      tapp (tident "print_endline") [transform_printer fn args]
    else if name = Names.debug then
      let stringified =
        args |> args_to_exprs |> List.map show_expr |> String.concat ", "
      in
      tapp (tident_ ["Printf"; "printf"])
        [tstr "%s: %s\n"; tstr stringified;
         implementations (List.hd Names.to_string) fn args]
    else fn

  let enter_expression e =
    let open Path in
    match e.exp_desc with
    | Texp_apply
        ({exp_desc =
            Texp_ident (Pdot (Pident {Ident.name = mod_name}, fn_name, _), _loc, _) }, args)
      when mod_name = Names.runtime ->
      implementations fn_name e args
    | Texp_ident (Pdot (Pident {Ident.name = mod_name}, fn_name, _), _loc, _)
      when mod_name = Names.runtime ->
      implementations fn_name e []
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
