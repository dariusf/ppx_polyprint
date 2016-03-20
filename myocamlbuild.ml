
open Ocamlbuild_plugin

let () =
  dispatch (
    function
    | After_rules ->
        flag ["ocaml"; "compile"; "ppx_native"] &
        S [A "-ppx"; A "src/ppx_polyprint.native"]
    | _ -> ())
