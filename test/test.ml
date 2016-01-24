
open PolyPrint

module Something : sig
  type t
  val thing : t
  val show_t : t -> string
end =
struct
  type t = int
  let thing = 1
  let show_t = string_of_int
end

type ('a, 'b) either = Left of 'a | Right of 'b

let show_either pr_a pr_b e =
  match e with
  | Left a -> "Left " ^ pr_a a
  | Right b -> "Right " ^ pr_b b

let () =
  (* simple *)
  assert (to_string 1 = "1");
  assert (to_string false = "false");
  assert (to_string "something" = "something");
  assert (to_string 'a' = "a");
  assert (to_string 1.2 = "1.2");
  assert (to_string (fun x -> x) = "<function>");
  assert (to_string (1, 2) = "(1, 2)");
  assert (to_string (1, 2, 3) = "(1, 2, 3)");
  assert (to_string (1, 2, 3, 4) = "(1, 2, 3, 4)");
  assert (to_string (1, 2, 3, 4, 5) = "(1, 2, 3, 4, 5)");
  assert (to_string (1, 2, 3, 4, 5, 6) = "(1, 2, 3, 4, 5, 6)");
  assert (to_string (1, 2, 3, 4, 5, 6, 7) = "(1, 2, 3, 4, 5, 6, 7)");
  assert (to_string [1; 2] = "[1; 2]");
  assert (to_string (Failure "what") = "Failure(\"what\")");

  (* compound *)
  assert (to_string [(1, 2)] = "[(1, 2)]");
  assert (to_string (1, (2, (3, (4, (5, (6, (7))))))) = "(1, (2, (3, (4, (5, (6, 7))))))");
  assert (to_string (1, "klasjd", true) = "(1, klasjd, true)");
  assert (to_string None = "None");
  assert (to_string (Some 1) = "Some 1");
  assert (to_string (Some "two") = "Some two");
  assert (to_string (Left 1) = "Left 1");
  assert (to_string (Right "hello") = "Right hello");

  (* qualified *)
  assert (to_string Something.thing = "1");
  assert (to_string Something.show_t = "<function>");

  (* aliases *)
  assert (string_of 1 = "1");
  print 1;
  print "something";

  (* type variables *)
  let id x =
    to_string x
  in
  assert (id 1 = "<polymorphic>");
  assert (id (None) = "<polymorphic>");

  let wrap x = Some x in
  assert (to_string (wrap 1) = "Some 1");
  assert (to_string (wrap None) = "Some None");

  let wrap x = to_string (Some x) in
  assert (to_string (wrap 1) = "Some <polymorphic>");
  assert (to_string (wrap None) = "Some <polymorphic>");

  print_endline "\n\nall good!\n\n"
