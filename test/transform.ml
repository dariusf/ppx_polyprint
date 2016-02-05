
open Test_util

let eta_expansion =
  let open PolyPrintShow in
  let eta_expand e =
    eta_expansion_mapper.Ast_mapper.expr eta_expansion_mapper e
  in
  let t = Pprintast.string_of_expression in
  List.map (fun (n, a, b) -> n, t a, t b) [
    ("identifier context",
     eta_expand [%expr PolyPrint.string_of],
     [%expr fun x -> PolyPrint.string_of x]);
    ("application context",
     eta_expand [%expr PolyPrint.string_of 1],
     [%expr PolyPrint.string_of 1]);
    ("qualified with non-existent name",
     eta_expand [%expr PolyPrint.x],
     [%expr fun x -> PolyPrint.x x]);
    ("unqualified",
     eta_expand [%expr string_of],
     [%expr string_of]);
  ]

let app_mapper =
  let open PolyPrintUtil in
  let transform find replace e =
    let mapper = app_mapper find replace in
    mapper.Ast_mapper.expr mapper e
  in
  let t = Pprintast.string_of_expression in
  List.map (fun (n, a, b) -> n, t a, t b) [
    ("ident", transform "a" "b" [%expr a], [%expr a]);
    ("app", transform "a" "b" [%expr a 1], [%expr b 1]);
    ("nested", transform "a" "b" [%expr fun x -> a 1], [%expr fun x -> b 1]);
  ]

let tests = [
  test_set "eta-expansion" eta_expansion;
  test_set "app mapper" app_mapper;
]
