
open Typedtree
open Types

open PPUtil

let too_many_args fn =
  failwith @@ Printf.sprintf "too many args given to %s!" (Untyped.show_expr fn)

let interpret (attrs : Parsetree.attributes) =
  let open Parsetree in
  let open Asttypes in
  match attrs with
  | [] -> None
  | ({ txt = name }, Parsetree.PStr str) :: _ ->
      if name <> Names.default_annotation then None
      else
        let check item =
          match item with
          | { pstr_desc = Pstr_eval ({
              pexp_desc = Pexp_construct ({ txt = path }, None)}, _) } ->
              [longident_to_list path]
          | _ -> []
        in
        begin
          match List.map check str |> List.concat with
          | x :: _ -> Some x (* TODO consider all, not just first *)
          | _ -> None
        end
  | _ -> None

(** Coerces a traced function into a regular function.
    TODO this is difficult to test, given the reliance on the type-checker closure.*)
let coerce untyped typed arg_count attrs check =
  let constructor, _texprs =
    (* TODO try to factor out this part *)
    match typed.exp_type.desc with
    | Tconstr (path_t, texprs, abbrev) ->
        begin
          let name =
            let open Path in
            match path_t with
            | Pident { Ident.name; _ } -> Some name, texprs
            | Pdot (Pident { Ident.name = prefix; _ }, name, _)
              when prefix = "Pervasives" ->
                Some name, texprs
            (* TODO check that it comes from polyprint *)
            | Pdot (prefix, t, _) -> (* TODO *)
                Some t, texprs
            | _ -> failwith "Papply not yet implemented"
          in name
        end
    | _ -> None, []
  in
  match constructor with
  | Some name when Names.is_traced_type name ->
      let arity = Names.traced_arity name in
      let untyped' =
        if arity < arg_count then
          too_many_args untyped
        else
          let loc = typed.exp_loc in
          let attr_name = interpret attrs in
          let config_module =
            match attr_name with
            | Some name -> name
            | _ -> otherwise
                     [Names.runtime; Names.default_module]
                     !PPEnv.specified_default_module in 
          let wrapped =
            Untyped.app ~loc
              (Untyped.qualified_ident ~loc
                 [Names.runtime; Names.wrap_n arity])
              [Untyped.location loc;
               Untyped.pack ~loc
                 config_module
                 [Names.runtime; Names.default_module_sig];
               untyped] [@metaloc loc] in

          if arity = arg_count then
            (* id 1 2 3 ==> (wrap id) 1 2 3 *)
            wrapped
          else
            (* id 1 2 ==> (fun a b c -> (wrap id) a b c) 1 2 *)
            Untyped.eta_abstract ~loc (arity - arg_count) wrapped
      in
      untyped', check untyped'
  | _ -> untyped, typed
