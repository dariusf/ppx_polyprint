
open Typedtree
open Types

open PPUtil

let too_many_args fn =
  failwith @@ Printf.sprintf "too many args given to %s!" (Untyped.show_expr fn)

(** Coerces a traced function into a regular function.
    TODO this is difficult to test, given the reliance on the type-checker closure.*)
let coerce untyped typed arg_count check =
  let constructor, texprs =
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
          let wrapped =
          Untyped.app
            (Untyped.qualified_ident
               [Names.runtime; Names.wrap_n arity]) [untyped]
            in
        if arity = arg_count then
          (* id 1 2 3 ==> (wrap id) 1 2 3 *)
          wrapped
        else
          (* id 1 2 ==> (fun a b c -> (wrap id) a b c) 1 2 *)
          Untyped.eta_abstract (arity - arg_count) wrapped
      in
      untyped', check untyped'
  | _ -> untyped, typed
