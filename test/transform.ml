
open Test_util

let eta_expansion =
  let open PolyPrintShow in
  let eta_expand e =
    eta_expansion_mapper.Ast_mapper.expr eta_expansion_mapper e
  in
  let t = Pprintast.string_of_expression in [
    ("identifier context",
     t @@ eta_expand [%expr PolyPrint.string_of],
     t @@ [%expr fun x -> PolyPrint.string_of x]);
    ("application context",
     t @@ eta_expand [%expr PolyPrint.string_of 1],
     t @@ [%expr PolyPrint.string_of 1]);
    ("qualified with non-existent name",
     t @@ eta_expand [%expr PolyPrint.x],
     t @@ [%expr fun x -> PolyPrint.x x]);
    ("unqualified",
     t @@ eta_expand [%expr string_of],
     t @@ [%expr string_of]);
  ]

let tests = [
  test_set "eta-expansion" eta_expansion;
]
