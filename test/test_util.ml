
let test_case name expected actual =
  name, `Quick, fun () -> Alcotest.(check string) name expected actual

let uncurry3 f (a, b, c) = f a b c

let test_set name tests =
  (name, List.map (uncurry3 test_case) tests)
  
