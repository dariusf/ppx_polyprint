
open Typedtree
open Types

open PPUtil

let coerce sfunct funct check =
  let constructor, texprs = match funct.exp_type.desc with
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
      let new_funct = Untyped.app
          (Untyped.qualified_ident
             [Names.runtime; Names.wrap_n arity]) [sfunct] in
      let funct = check new_funct in
      new_funct, funct
  | _ -> sfunct, funct
