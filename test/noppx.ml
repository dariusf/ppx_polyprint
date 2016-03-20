
open Test_util
open Ast_mapper
open PPUtil
open PPUtil.Untyped

let eta_expansion =
  let open PPShow in
  let eta_expand e =
    eta_expansion_mapper.expr eta_expansion_mapper e
  in
  let t = Pprintast.string_of_expression in
  List.map (fun (n, a, b) -> n, t a, t b) [
    ("identifier context",
     eta_expand [%expr PolyPrint.string_of],
     [%expr fun x -> PolyPrint.string_of x]);
    ("application context",
     eta_expand [%expr PolyPrint.string_of 1],
     [%expr PolyPrint.string_of 1]);
    ("qualified, non-existent identifier",
     eta_expand [%expr PolyPrint.x],
     [%expr fun x -> PolyPrint.x x]);
    ("unqualified",
     eta_expand [%expr string_of],
     [%expr string_of]);
    ("qualified argument to qualified function",
     eta_expand [%expr PolyPrint.x PolyPrint.string_of],
     [%expr PolyPrint.x (fun x -> PolyPrint.string_of x)]);
  ]

let app_mapper =
  let transform find replace e =
    let mapper = app_mapper find replace in
    mapper.expr mapper e
  in
  let t = Pprintast.string_of_expression in
  List.map (fun (n, a, b) -> n, t a, t b) [
    ("ident", transform "a" "b" [%expr a], [%expr a]);
    ("app", transform "a" "b" [%expr a 1], [%expr b 1]);
    ("nested", transform "a" "b" [%expr fun x -> a 1], [%expr fun x -> b 1]);
  ]

let idempotence_of_serialisation =
  let open Yojson.Basic in [
    begin
      let s = "{\"default\":[\"a\",\"module\"],\"functions\":{\"name\":[\"mod1\",\"mod2\"],\"name1\":[\"mod3\"]}}" in
      PPEnv.init ();
      PPEnv.from_json (from_string s);
      let s2 = to_string @@ PPEnv.to_json () in
      ("well-formed", s, s2)
    end;

    begin
      let s = "{\"default\":[\"a\",\"module\"]}" in
      PPEnv.init ();
      PPEnv.from_json (from_string s);
      let s2 = to_string @@ PPEnv.to_json () in
      ("missing fields", s, s2)
    end;

    begin
      let s = "{\"default\":\"a\"}" in
      PPEnv.init ();
      PPEnv.from_json (from_string s);
      let s2 = to_string @@ PPEnv.to_json () in
      PPEnv.init ();
      let s3 = to_string @@ PPEnv.to_json () in
      ("ill-formed", s3, s2)
    end;
  ]

let tests = [
  test_set "eta-expansion" eta_expansion;
  test_set "app mapper" app_mapper;
  test_set "idempotence of serialisation mapper" idempotence_of_serialisation;
]
