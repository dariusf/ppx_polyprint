
open Test_util
open Ast_mapper
open PolyPrintUtil

let eta_expansion =
  let open PolyPrintShow in
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

(* let binding_transforms = *)
(*   let open Ast_helper in *)
(*   let open Asttypes in *)
(*   let open Parsetree in *)
(*   let open PolyPrintLog in *)
(*   let wrap r binding = *)
(*     Exp.let_ r [binding] [%expr 1] *)
(*   in *)
(*   let t = Pprintast.string_of_expression in *)
(*   List.map (fun (n, a, b) -> n, t a, t b) [ *)
(*     ("nonrec transform, nonrec function", *)
(*      wrap Nonrecursive *)
(*        (transform_nonrecursive_binding @@ binding (pat_var "plus") [%expr *)
(*           fun x y -> x + y]), *)
(*      wrap Nonrecursive *)
(*        (binding (pat_var "plus") [%expr *)
(*           fun x y -> *)
(*             let plus_original x y = x + y in *)
(*             let result = plus_original x y in *)
(*             (PolyPrint.print result; result)])); *)
(*     ("nonrec transform, rec function", *)
(*      wrap Recursive *)
(*        (transform_nonrecursive_binding @@ binding (pat_var "plus") [%expr *)
(*           fun x -> plus x]), *)
(*      wrap Recursive *)
(*        (binding (pat_var "plus") [%expr  *)
(*           fun x ->  *)
(*             let plus_original x = plus x in *)
(*             let result = plus_original x in *)
(*             PolyPrint.print result; result])); *)
(*     ("rec transform, nonrec function", *)
(*      wrap Nonrecursive *)
(*        (transform_recursive_binding @@ binding (pat_var "plus") [%expr *)
(*           fun x -> x]), *)
(*      wrap Nonrecursive *)
(*        (binding (pat_var "plus") [%expr  *)
(*           fun x ->  *)
(*             let plus_original self x = x in *)
(*             let rec aux x = *)
(*               Printf.printf "plus = x: %s\n" (PolyPrint.to_string x); *)
(*               (let result = plus_original aux x in PolyPrint.print result; result) in *)
(*             aux x])); *)
(*     ("rec transform, rec function", *)
(*      wrap Recursive *)
(*        (transform_recursive_binding @@ binding (pat_var "fact") [%expr *)
(*           fun n -> if n = 1 then 0 else n * fact (n - 1)]), *)
(*      wrap Recursive *)
(*        (binding (pat_var "fact") [%expr  *)
(*           fun n ->  *)
(*             let fact_original self n = if n = 1 then 0 else n * (self (n - 1)) in *)
(*             let rec aux n = *)
(*               Printf.printf "fact = n: %s\n" (PolyPrint.to_string n); *)
(*               (let result = fact_original aux n in PolyPrint.print result; result) in *)
(*             aux n])); *)
(*   ] *)

let tests = [
  test_set "eta-expansion" eta_expansion;
  test_set "app mapper" app_mapper;
  (* test_set "binding transforms" binding_transforms; *)
]
