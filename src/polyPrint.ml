
let to_string : 'a -> string =
  fun _ -> raise (Failure "ppx_polyprint not set up properly?")

let string_of = to_string

let show = to_string

let print x = x |> to_string |> print_endline

let debug = print

type param_name = string
type fn_name = string
type value = string
type file_path = string
type line_number = int

type 'a printer = 'a -> string
type 'a param_spec = param_name * 'a printer * 'a
type loc = file_path * line_number

module type TraceConfig = sig

  (** High-level API, for configuring how tracing is performed. *)

  val sep : unit -> string
  val fn : fn_name -> string
  val arg : param_name -> value -> string
  val result : fn_name -> value -> string

  (** Low-level API, for tweaks that fundamentally change how function
      tracing is done. All the different arities need to be implemented
      for consistency.

      There is no conceptual difference between all these functions of
      different arity: a function may be of any arity, depending on how
      many parameters are being tracked. *)

  val print_result : string -> 'a printer -> 'a -> unit

  val print_args1 : string ->
    'a param_spec -> unit
  val print_args2 : string ->
    'a param_spec ->
    'b param_spec -> unit
  val print_args2 : string ->
    'a param_spec ->
    'b param_spec -> unit
  val print_args3 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec -> unit
  val print_args4 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec -> unit
  val print_args5 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec -> unit
  val print_args6 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec ->
    'f param_spec -> unit
  val print_args7 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec ->
    'f param_spec ->
    'g param_spec -> unit

  val run1 : string ->
    'a param_spec ->
    'b printer -> ('a -> 'b) -> 'b
  val run2 : string ->
    'a param_spec ->
    'b param_spec ->
    'c printer -> ('a -> 'b -> 'c) -> 'c
  val run3 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd printer -> ('a -> 'b -> 'c -> 'd) -> 'd
  val run4 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e printer -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e
  val run5 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec ->
    'f printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'f
  val run6 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec ->
    'f param_spec ->
    'g printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'g
  val run7 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec ->
    'f param_spec ->
    'g param_spec ->
    'h printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'h

  val call1 : fn_name -> loc -> ('a -> 'b) -> 'a -> 'b
  val call2 : fn_name -> loc -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  val call3 : fn_name -> loc -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
  val call4 : fn_name -> loc -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
  val call5 : fn_name -> loc -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
  val call6 : fn_name -> loc -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
  val call7 : fn_name -> loc -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
end

module DefaultTraceConfig : TraceConfig = struct
  open Printf

  (* TODO add an implementation that tracks recursion depth *)
  (* TODO demo stubbing these out *)
  (* TODO make this use debug.ml *)

  let sep () = " | "
  let fn name = name ^ " <-"
  let arg name value = sprintf "%s = %s" name value
  let result fn_name value = sprintf "%s -> %s" fn_name value

  let print_result fn_name pr_res res =
    print_endline (result fn_name (pr_res res))

  let print_args1 fn_name (a_n, pr_a, a) =
    printf "%s %s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a))])
  let print_args2 fn_name (a_n, pr_a, a) (b_n, pr_b, b) =
    printf "%s %s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b))])
  let print_args3 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) =
    printf "%s %s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c))])
  let print_args4 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) =
    printf "%s %s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d))])
  let print_args5 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) =
    printf "%s %s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d)); (arg e_n (pr_e e))])
  let print_args6 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) =
    printf "%s %s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d)); (arg e_n (pr_e e)); (arg f_n (pr_f f))])
  let print_args7 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) (g_n, pr_g, g) =
    printf "%s %s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d)); (arg e_n (pr_e e)); (arg f_n (pr_f f)); (arg g_n (pr_g g))])

  let run1 fn_name ((_, _, a) as aa) pr_res fn =
    print_args1 fn_name aa;
    let res = fn a in
    print_result fn_name pr_res res;
    res

  let run2 fn_name ((_, _, a) as aa) ((_, _, b) as bb) pr_res fn =
    print_args2 fn_name aa bb;
    let res = fn a b in
    print_result fn_name pr_res res;
    res

  let run3 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc) pr_res fn =
    print_args3 fn_name aa bb cc;
    let res = fn a b c in
    print_result fn_name pr_res res;
    res

  let run4 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) pr_res fn =
    print_args4 fn_name aa bb cc dd;
    let res = fn a b c d in
    print_result fn_name pr_res res;
    res

  let run5 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) ((_, _, e) as ee) pr_res fn =
    print_args5 fn_name aa bb cc dd ee;
    let res = fn a b c d e in
    print_result fn_name pr_res res;
    res

  let run6 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) ((_, _, e) as ee) ((_, _, f) as ff) pr_res fn =
    print_args6 fn_name aa bb cc dd ee ff;
    let res = fn a b c d e f in
    print_result fn_name pr_res res;
    res

  let run7 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) ((_, _, e) as ee) ((_, _, f) as ff)
      ((_, _, g) as gg) pr_res fn =
    print_args7 fn_name aa bb cc dd ee ff gg;
    let res = fn a b c d e f g in
    print_result fn_name pr_res res;
    res

  let call1 _ _ fn a = fn a
  let call2 _ _ fn a b = fn a b
  let call3 _ _ fn a b c = fn a b c
  let call4 _ _ fn a b c d = fn a b c d
  let call5 _ _ fn a b c d e = fn a b c d e
  let call6 _ _ fn a b c d e f = fn a b c d e f
  let call7 _ _ fn a b c d e f g = fn a b c d e f g
end

module Printers = struct

  let string_of_int = string_of_int
  let string_of_int32 = Int32.to_string
  let string_of_int64 = Int64.to_string
  let string_of_nativeint = Nativeint.to_string

  let string_of_bool = string_of_bool
  let string_of_float = string_of_float

  let string_of_char = String.make 1
  let string_of_exn = Printexc.to_string

  open Format

  let pp_tvar name fmt _ =
    match name with
    | "" -> fprintf fmt "<polymorphic>"
    | v -> fprintf fmt "'%s" v

  let pp_option pp fmt x =
    match x with
    | Some s -> fprintf fmt "Some %a" pp s
    | None -> fprintf fmt "None"

  let pp_ref pp fmt x =
    fprintf fmt "ref %a" pp !x

  let pp_unit fmt x =
    fprintf fmt "()"

  let pp_exc fmt x =
    fprintf fmt "%s" (Printexc.to_string x)

  let pp_int32 fmt x =
    fprintf fmt "%ld" x

  let pp_int64 fmt x =
    fprintf fmt "%Ld" x

  let pp_nativeint fmt x =
    fprintf fmt "%nd" x

  let rec pp_list pp fmt xs =
    let rec aux xs =
      match xs with
      | [] -> fprintf fmt ""
      | [x] -> fprintf fmt "%a" pp x
      | y :: ys -> fprintf fmt "%a; " pp y; aux ys
    in
    fprintf fmt "[";
    aux xs;
    fprintf fmt "]"

  let pp_function fmt _ = fprintf fmt "<function>"

  let pp_function_rep f fmt _ = fprintf fmt "<function %s>" f

  let pp_misc s fmt _ = fprintf fmt "%s" s

  let pp_tuple pr_a pr_b fmt (a, b) =
    fprintf fmt "(%a, %a)" pr_a a pr_b b
  let pp_tuple3 pr_a pr_b pr_c fmt (a, b, c) =
    fprintf fmt "(%a, %a, %a)" pr_a a pr_b b pr_c c
  let pp_tuple4 pr_a pr_b pr_c pr_d fmt (a, b, c, d) =
    fprintf fmt "(%a, %a, %a, %a)" pr_a a pr_b b pr_c c pr_d d
  let pp_tuple5 pr_a pr_b pr_c pr_d pr_e fmt (a, b, c, d, e) =
    fprintf fmt "(%a, %a, %a, %a, %a)" pr_a a pr_b b pr_c c pr_d d pr_e e
  let pp_tuple6 pr_a pr_b pr_c pr_d pr_e pr_f fmt (a, b, c, d, e, f) =
    fprintf fmt "(%a, %a, %a, %a, %a, %a)" pr_a a pr_b b pr_c c pr_d d pr_e e pr_f f
  let pp_tuple7 pr_a pr_b pr_c pr_d pr_e pr_f pr_g fmt (a, b, c, d, e, f, g) =
    fprintf fmt "(%a, %a, %a, %a, %a, %a, %a)" pr_a a pr_b b pr_c c pr_d d pr_e e pr_f f pr_g g

  (* let string_of_tvar name _ = *)
  (*   match name with *)
  (*   | "" -> "<polymorphic>" *)
  (*   | v -> "'" ^ v *)

  (* let string_of_option pr x = *)
  (*   match x with *)
  (*   | Some s -> "Some " ^ pr s *)
  (*   | None -> "None" *)

  (* let string_of_ref pr x = "ref " ^ pr !x *)

  (* let string_of_list pr xs = *)
  (*   let rec aux xs = *)
  (*     match xs with *)
  (*     | [] -> "" *)
  (*     | [x] -> pr x *)
  (*     | y :: ys -> pr y ^ "; " ^ aux ys *)
  (*   in "[" ^ aux xs ^ "]" *)

  (* let string_of_function _ = "<function>" *)

  (* open Printf *)

  (* let string_of_tuple pr_a pr_b (a, b) = *)
  (*   sprintf "(%s, %s)" (pr_a a) (pr_b b) *)
  (* let string_of_tuple3 pr_a pr_b pr_c (a, b, c) = *)
  (*   sprintf "(%s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) *)
  (* let string_of_tuple4 pr_a pr_b pr_c pr_d (a, b, c, d) = *)
  (*   sprintf "(%s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d) *)
  (* let string_of_tuple5 pr_a pr_b pr_c pr_d pr_e (a, b, c, d, e) = *)
  (*   sprintf "(%s, %s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d) (pr_e e) *)
  (* let string_of_tuple6 pr_a pr_b pr_c pr_d pr_e pr_f (a, b, c, d, e, f) = *)
  (*   sprintf "(%s, %s, %s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d) (pr_e e) (pr_f f) *)
  (* let string_of_tuple7 pr_a pr_b pr_c pr_d pr_e pr_f pr_g (a, b, c, d, e, f, g) = *)
  (*   sprintf "(%s, %s, %s, %s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d) (pr_e e) (pr_f f) (pr_g g) *)

end
