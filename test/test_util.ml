
let test_case name expected actual =
  name, `Quick, fun () -> Alcotest.(check string) name expected actual

let basic_test_case name actual =
  name, `Quick, fun () -> Alcotest.(check bool) name true actual

let uncurry2 f (a, b) = f a b

let uncurry3 f (a, b, c) = f a b c

let test_set name tests =
  (name, List.map (uncurry3 test_case) tests)
  
let basic_test_set name tests =
  (name, List.map (uncurry2 basic_test_case) tests)
