
open Test_util
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

  type 'a s = Cons of 'a * 'a s | Nil
  val show_s : ('a -> string) -> 'a s -> string
end =
struct
  type t = int
  let thing = 1
  let show_t = string_of_int

  type 'a s = Cons of 'a * 'a s | Nil
  let rec show_s pr s =
  match s with
  | Cons (a, rest) -> pr a ^ " " ^ show_s pr rest
  | Nil -> ""
end

let qualified = [
  "qualified abstract value", to_string Something.show_t, "<function>";
  "qualified abstract value t defaults to show_t", to_string Something.thing, "1";
  "qualified parameterised abstract value", Something.(to_string (Cons (1, Cons (2, Nil)))), "1 2 ";
  "qualified parameterised abstract value", Something.(to_string (Cons ("a", Cons ("b", Nil)))), "a b ";
]

let type_variables =
  let string_form_of x = to_string x in
  let wrap_and_stringify x = to_string (Some x) in [
    "polymorphic to_string 1", string_form_of 1, "<polymorphic>";
    "polymorphic to_string 2", string_form_of (None), "<polymorphic>";
    "polymorphic to_string 3", to_string (wrap_and_stringify 1), "Some <polymorphic>";
    "polymorphic to_string 4", to_string (wrap_and_stringify None), "Some <polymorphic>";
  ]

let higher_order =
  let showp pr a = pr a in [
    "intended transformation", showp (fun x -> to_string x) 1, "1";
    "int", showp PolyPrint.to_string 1, "1";
    "string", showp PolyPrint.to_string "aaa", "aaa";
    "aliases 1", showp PolyPrint.string_of "aaa", "aaa";
    "aliases 2", showp PolyPrint.show "aaa", "aaa";
    "unqualified", showp to_string 1, "'a";
  ]

module Counter = struct
  include PolyPrint.Default

  let count = ref 0

  let run1 fn_name (a_n, pr_a, a) pr_res f =
    incr count; f a

  let run2 fn_name (a_n, pr_a, a) (b_n, pr_b, b) pr_res f =
    incr count; f a b
end

let rec fact_str n =
  if n = 0 then 1 else n * fact_str (n - 1)
  [@@log Counter]

let plus_str a b = a + b
  [@@log Counter]

let rec fact_str_rec n =
  if n = 0 then 1 else n * fact_str_rec (n - 1)
  [@@logrec Counter]

(* These will fail, but can't really be tested... *)

(* let plus_str_rec a b = a + b *)
(* [@@logrec Counter] *)

(* let () = *)
(* let [@logrec Counter] plus_expr_rec a b = a + b in () *)

let logging =
  let [@log Counter] rec fact_expr n =
    if n = 0 then 1 else n * fact_expr (n - 1)
  in
  let [@log Counter] plus_expr a b = a + b in
  let [@logrec Counter] rec fact_expr_rec n =
    if n = 0 then 1 else n * fact_expr_rec (n - 1)
  in
  let open Counter in [
    ("recursive log expression",
     let current = !count in
     ignore (fact_expr 5);
     print !count;
     (!count - current) = 1);

    ("recursive log structure",
     let current = !count in
     ignore (fact_str 5);
     print !count;
     (!count - current) = 1);

    ("recursive logrec expression",
     let current = !count in
     ignore (fact_expr_rec 5);
     (!count - current) = 6);

    ("recursive logrec structure",
     let current = !count in
     ignore (fact_str_rec 5);
     (!count - current) = 6);

    ("non-recursive log expression",
     let current = !count in
     ignore (plus_expr 2 3);
     (!count - current) = 1);

    ("non-recursive log structure",
     let current = !count in
     ignore (plus_str 2 3);
     (!count - current) = 1);
  ]

let tests = [
  test_set "simple" simple;
  test_set "compound" compound;
  test_set "qualified" qualified;
  test_set "type variables" type_variables;
  test_set "higher-order" higher_order;
  basic_test_set "logging" logging;
]
