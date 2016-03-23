
open Test_util
open PolyPrint
open Format

type record = { a : int; b : string }
(* [@@deriving show] *)

(* this can be removed when the typpx-deriving bug is fixed *)
let pp_record fmt { a; b } =
  fprintf fmt "{ a = %d; b = %s }" a b

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

(* this needs to be defined *)
let pp_some_class fmt sc =
  fprintf fmt "%s" sc#to_string

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
  "tuple", to_string (1, 2), "(1, 2)";
  "tuple3", to_string (1, 2, 3), "(1, 2, 3)";
  "tuple4", to_string (1, 2, 3, 4), "(1, 2, 3, 4)";
  "tuple5", to_string (1, 2, 3, 4, 5), "(1, 2, 3, 4, 5)";
  "tuple6", to_string (1, 2, 3, 4, 5, 6), "(1, 2, 3, 4, 5, 6)";
  "tuple7", to_string (1, 2, 3, 4, 5, 6, 7), "(1, 2, 3, 4, 5, 6, 7)";
  "int list", to_string [1; 2], "[1; 2]";
  "bool list", to_string [true; false], "[true; false]";
  "ref", to_string (ref 1), "ref 1";
  "object (of class)", to_string (new some_class), "this is some class";
  "object (singleton)", (to_string singleton_obj), "< meth : int >";
  "exn", to_string (Failure "what"), "Failure(\"what\")";
  "record", to_string { a = 1; b = "hi" }, "{ a = 1; b = hi }";
  "int32", to_string (Int32.of_int 45), "45";
  "int64", to_string (Int64.of_int 74), "74";
  "nativeint", to_string (Nativeint.of_int 65), "65";
  "unit", to_string (), "()";
]

let functions = [
  "function literal", show (fun x -> x), "<function fun x -> x : 'a -> 'a>";
  "function variable", (let y x = x in show y), "<function y : 'a -> 'a>";
]

type ('a, 'b) either = Left of 'a | Right of 'b
  [@@deriving show]

let compound = [
  "int tuple list", to_string [(1, 2)], "[(1, 2)]";
  "nested tuples", to_string (1, (2, (3, 4))), "(1, (2, (3, 4)))";
  "heterogeous tuples", to_string (1, "klasjd", true), "(1, klasjd, true)";
  "option none", to_string None, "None";
  "option int", to_string (Some 1), "Some 1";
  "option string", to_string (Some "two"), "Some two";
  "user-defined either 1 (defaults to show_either)", to_string (Left 1), "(Ppx.Left 1)";
  "user-defined either 2", to_string (Right "hello"), "(Ppx.Right hello)";
]

module Something : sig
  type t
  val thing : t
  val pp_t : Format.formatter -> t -> unit

  val id : t -> t

  type 'a s = Cons of 'a * 'a s | Nil

  val pp_s : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a s -> unit
end =
struct
  type t = int

  (* if we derive show, we can't constrain the module via its signature
     as the printers will be absent. *)
  (* [@@deriving show] *)

  let pp_t = pp_print_int

  let thing = 1

  type 'a s = Cons of 'a * 'a s | Nil
    [@@deriving show]

  let id x = x

  let rec pp_s pp fmt xs =
    let rec aux xs =
      match xs with
      | Nil -> fprintf fmt ""
      | Cons (x, Nil) -> fprintf fmt "%a" pp x
      | Cons (y, ys) -> fprintf fmt "%a; " pp y; aux ys
    in
    fprintf fmt "[";
    aux xs;
    fprintf fmt "]"
end

let qualified = [
  "qualified abstract value", to_string Something.id, "<function Something.id : Something.t -> Something.t>";
  "qualified abstract value t defaults to show_t", to_string Something.thing, "1";
  "qualified parameterised abstract value", Something.(to_string (Cons (1, Cons (2, Nil)))), "[1; 2]";
  "qualified parameterised abstract value", Something.(to_string (Cons ("a", Cons ("b", Nil)))), "[a; b]";
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

module TestConfig = struct
  include DefaultTraceConfig

  let count = ref 0
  let last = ref 0

  let called = ref false

  let reset () =
    count := 0;
    last := 0;
    called := false

  let run1 fn_name (a_n, pr_a, a) pr_res f =
    incr count; last := 1; f a

  let run2 fn_name (a_n, pr_a, a) (b_n, pr_b, b) pr_res f =
    incr count; last := 2; f a b

  let call1 _ _ f a =
    called := true; f a
end

module Otherwise = struct
  module Default = struct
    include DefaultTraceConfig

    let called = ref false

    let reset () =
      called := false

    let call1 _ _ f a =
      called := true; f a
  end
end

let rec fact_str n =
  if n = 0 then 1 else n * fact_str (n - 1)
  [@@trace TestConfig]

let plus_str a b = a + b
  [@@trace TestConfig]

let rec fact_str_rec n =
  if n = 0 then 1 else n * fact_str_rec (n - 1)
  [@@tracerec TestConfig]

(* These will fail, but can't really be tested... *)

(* let plus_str_rec a b = a + b *)
(* [@@tracerec TestConfig] *)

(* let () = *)
(* let [@tracerec TestConfig] plus_expr_rec a b = a + b in () *)

(* has to be here for now, or the test fails *)
[@@@polyprint Otherwise.Default]

let tracing =
  let [@trace TestConfig] rec fact_expr n =
    if n = 0 then 1 else n * fact_expr (n - 1)
  in
  let [@trace TestConfig] plus_expr a b = a + b in
  let [@tracerec TestConfig] rec fact_expr_rec n =
    if n = 0 then 1 else n * fact_expr_rec (n - 1)
  in
  let open TestConfig in [
    ("recursive trace expression",
     begin
       reset ();
       ignore (fact_expr 5);
       !count = 1
     end);

    ("recursive trace structure",
     begin
       reset ();
       ignore (fact_str 5);
       !count  = 1
     end);

    ("recursive tracerec expression",
     begin
       reset ();
       ignore (fact_expr_rec 5);
       !count = 6
     end);

    ("recursive tracerec structure",
     begin
       reset ();
       ignore (fact_str_rec 5);
       !count = 6
     end);

    ("non-recursive trace expression",
     begin
       reset ();
       ignore (plus_expr 2 3);
       !count = 1
     end);

    ("non-recursive trace structure",
     begin
       reset ();
       ignore (plus_str 2 3);
       !count = 1
     end);

    ("non-recursive trace structure",
     begin
       reset ();
       ignore (plus_str 2 3);
       !count = 1
     end);

    ("variable filtering",
     let [@tracerec TestConfig; a] rec var_filtering1 a b c d =
       if a = 4 then 5
       else var_filtering1 b c d a
     in
     let [@tracerec TestConfig; a, b] rec var_filtering2 a b c d =
       if a = 4 then 5
       else var_filtering2 b c d a
     in
     reset ();
     let a = !last = 0 in
     ignore (var_filtering1 1 2 3 4);
     let b = !last = 1 in
     ignore (var_filtering2 1 2 3 4);
     let c = !last = 2 in
     a && b && c);

    ("call wrapping",
     let [@trace TestConfig] rather_unique x = x in
     reset ();
     let a = not !called in
     ignore @@ rather_unique 1;
     let b = !called in
     a && b);

    ("default annotation",
     let [@trace] id x = x in
     Otherwise.Default.reset ();
     let a = not !Otherwise.Default.called in
     ignore @@ id 1;
     let b = !Otherwise.Default.called in
     a && b);
  ]

let tests = [
  test_set "simple" simple;
  test_set "functions" functions;
  test_set "compound" compound;
  test_set "qualified" qualified;
  test_set "type variables" type_variables;
  test_set "higher-order" higher_order;
  basic_test_set "tracing" tracing;
]
