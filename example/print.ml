

open PolyPrint

;;

print 1;
print true;
print "hi";
print 'c';
print 1.2;
print (1, 1);
print (1, (2, (3, (4, (5, (6, 7))))));
print [1; 2];
print [(1, 2); (3, 4)];
print [true; false];
print (Failure "what");

;;

type record = { a : int; b : string }

let show_record { a; b } =
  "{ a = " ^ string_of_int a ^ "; b = " ^ b ^ " }"

;;

print { a = 1; b = "hi" };

;;

type 'a maybe = Just of 'a | Nothing
  [@@deriving show]
(* type ('a, 'b) either = Left of 'a | Right of 'b *)
  (* [@@deriving show] *)

;;

(* print (Left 1); *)
(* print (Left true); *)

(* print (Just 1); *)

(* ;; *)

let string_of x = to_string x

;;

print (string_of 1);
print (string_of None);

;;

let wrap x = Some (to_string x)

;;

print (wrap None)
