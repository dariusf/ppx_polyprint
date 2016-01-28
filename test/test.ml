
open PolyPrint

let test_case group name expected actual =
  group, `Quick, fun () -> Alcotest.(check string) name expected actual

type record = { a : int; b : string }

let show_record { a; b } =
  "{ a = " ^ string_of_int a ^ "; b = " ^ b ^ " }"

let simple = [
  "int", to_string 1, "1";
  "bool", to_string false, "false";
  "string", to_string "something", "something";
  "char", to_string 'a', "a";
  "float", to_string 1.2, "1.2";
  "function", to_string (fun x -> x), "<function>";
  "tuple", to_string (1, 2), "(1, 2)";
  "tuple3", to_string (1, 2, 3), "(1, 2, 3)";
  "tuple4", to_string (1, 2, 3, 4), "(1, 2, 3, 4)";
  "tuple5", to_string (1, 2, 3, 4, 5), "(1, 2, 3, 4, 5)";
  "tuple6", to_string (1, 2, 3, 4, 5, 6), "(1, 2, 3, 4, 5, 6)";
  "tuple7", to_string (1, 2, 3, 4, 5, 6, 7), "(1, 2, 3, 4, 5, 6, 7)";
  "int list", to_string [1; 2], "[1; 2]";
  "bool list", to_string [true; false], "[true; false]";
  "exn", to_string (Failure "what"), "Failure(\"what\")";
  "record", to_string { a = 1; b = "hi" }, "{ a = 1; b = hi }";
]

type ('a, 'b) either = Left of 'a | Right of 'b

let show_either pr_a pr_b e =
  match e with
  | Left a -> "Left " ^ pr_a a
  | Right b -> "Right " ^ pr_b b

let compound = [
  "int tuple list", to_string [(1, 2)], "[(1, 2)]";
  "nested tuples", to_string (1, (2, (3, 4))), "(1, (2, (3, 4)))";
  "heterogeous tuples", to_string (1, "klasjd", true), "(1, klasjd, true)";
  "option none", to_string None, "None";
  "option int", to_string (Some 1), "Some 1";
  "option string", to_string (Some "two"), "Some two";
  "user-defined either 1 (defaults to show_either)", to_string (Left 1), "Left 1";
  "user-defined either 2", to_string (Right "hello"), "Right hello";
]

module Something : sig
  type t
  val thing : t
  val show_t : t -> string

  type 'a s = Cons of 'a * 'a s | Nil
  (* val thing_s : 'a s *)
  val show_s : ('a -> string) -> 'a s -> string
end =
struct
  type t = int
  let thing = 1
  let show_t = string_of_int

  type 'a s = Cons of 'a * 'a s | Nil
  (* let thing_s : int s = Cons (1, Cons (2, Nil)) *)
  let rec show_s pr s =
    match s with
    | Cons (a, rest) -> pr a ^ " " ^ show_s pr rest
    | Nil -> ""
end

let qualified = [
  "qualified abstract value", to_string Something.show_t, "<function>";
  "qualified abstract value t defaults to show_t", to_string Something.thing, "1";
  "qualified parameterised abstract value",
  Something.(to_string (Cons (1, Cons (2, Nil)))), "1 2 ";
]

let string_form_of x = to_string x
let wrap_and_stringify x = to_string (Some x)

let type_variables = [
  "polymorphic to_string 1", string_form_of 1, "<polymorphic>";
  "polymorphic to_string 2", string_form_of (None), "<polymorphic>";
  "polymorphic to_string 3", to_string (wrap_and_stringify 1), "Some <polymorphic>";
  "polymorphic to_string 4", to_string (wrap_and_stringify None), "Some <polymorphic>";
]

let uncurry3 f (a, b, c) = f a b c

let tests =
  let open List in [
  map (uncurry3 (test_case "simple")) simple;
  map (uncurry3 (test_case "compound")) compound;
  map (uncurry3 (test_case "qualified")) qualified;
  map (uncurry3 (test_case "type variables")) type_variables;
] |> concat

let () =
  Alcotest.run "ppx_polyprint" ["all", tests]
