
open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    flag ["ocaml"; "compile"; "polyprint_byte"] &
      S [A "-ppx"; A "$(ocamlfind query ppx_polyprint)/ppx_polyprint.byte"];
    flag ["ocaml"; "compile"; "polyprint_native"] &
      S [A "-ppx"; A "$(ocamlfind query ppx_polyprint)/ppx_polyprint.native"]
  | _ -> ())
