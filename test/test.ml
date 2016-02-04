
open PolyPrint

type record = { a : int; b : string }

let show_record { a; b } =
  "{ a = " ^ string_of_int a ^ "; b = " ^ b ^ " }"

class some_class =
  object (self)
    val field = 1
    method meth x =
      x + 1
    method meth2 =
      field
    method to_string =
      "this is some class"
  end

let show_some_class sc =
  sc#to_string

let singleton_obj = object
  val v = 1
  method meth = v
end

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
  "ref", to_string (ref 1), "ref 1";
  "object", to_string (new some_class), "this is some class";
  "object", (to_string singleton_obj), "< meth : int >";
  "int32", (to_string (Int32.of_int 45)), "45";
  "int64", (to_string (Int64.of_int 74)), "74";
  "nativeint", (to_string (Nativeint.of_int 65)), "65";
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

  (* type 'a s = Cons of 'a * 'a s | Nil *)
  (* val thing_s : 'a s *)
  (* val show_s : ('a -> string) -> 'a s -> string *)
end =
struct
  type t = int
  let thing = 1
  let show_t = string_of_int

  (* type 'a s = Cons of 'a * 'a s | Nil *)
  (* let thing_s : int s = Cons (1, Cons (2, Nil)) *)
  (* let rec show_s pr s = *)
    (* match s with *)
    (* | Cons (a, rest) -> pr a ^ " " ^ show_s pr rest *)
    (* | Nil -> "" *)
end

let qualified = [
  "qualified abstract value", to_string Something.show_t, "<function>";
  "qualified abstract value t defaults to show_t", to_string Something.thing, "1";
  (* "qualified parameterised abstract value", Something.(to_string (Cons (1, Cons (2, Nil)))), "1 2 "; *)
  (* "qualified parameterised abstract value", Something.(to_string (Cons ("a", Cons ("b", Nil)))), "a b "; *)
]

let string_form_of x = to_string x
let wrap_and_stringify x = to_string (Some x)

let type_variables = [
  "polymorphic to_string 1", string_form_of 1, "<polymorphic>";
  "polymorphic to_string 2", string_form_of (None), "<polymorphic>";
  "polymorphic to_string 3", to_string (wrap_and_stringify 1), "Some <polymorphic>";
  "polymorphic to_string 4", to_string (wrap_and_stringify None), "Some <polymorphic>";
]

let test_case name expected actual =
  name, `Quick, fun () -> Alcotest.(check string) name expected actual

let uncurry3 f (a, b, c) = f a b c

let test_set name tests =
  (name, List.map (uncurry3 test_case) tests)
  
let () =
  Alcotest.run "ppx_polyprint" [
    test_set "simple" simple;
    test_set "compound" compound;
    test_set "qualified" qualified;
    test_set "type variables" type_variables;
  ]
