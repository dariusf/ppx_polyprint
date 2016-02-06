
let to_string : 'a -> string =
  fun _ -> raise (Failure "ppx_polyprint not set up properly?")

let string_of = to_string

let show = to_string

let print x = x |> to_string |> print_endline

type param_name = string
type 'a printer = 'a -> string
type 'a param_spec = param_name * 'a printer * 'a

module type LogSpec = sig

  (** High-level API, for configuring how logging appears. *)

  val sep : unit -> string
  val fn : string -> string
  val arg : string -> string -> string
  val result : string -> string -> string

  (** Low-level API, for tweaks that fundamentally change how logging
      is carried out. All the different arities need to be implemented
      for consistency.

      There is no conceptual difference between all these functions of
      different arity: a function may be of any arity, depending on how
      many parameters are being logged. *)

  val print_result : string -> 'a printer -> 'a -> unit

  val print1 : string ->
    'a param_spec -> unit
  val print2 : string ->
    'a param_spec ->
    'b param_spec -> unit
  val print2 : string ->
    'a param_spec ->
    'b param_spec -> unit
  val print3 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec -> unit
  val print4 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec -> unit
  val print5 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec -> unit
  val print6 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec ->
    'f param_spec -> unit
  val print7 : string ->
    'a param_spec ->
    'b param_spec ->
    'c param_spec ->
    'd param_spec ->
    'e param_spec ->
    'f param_spec ->
    'g param_spec -> unit
end

module Default : LogSpec = struct
  open Printf

  (* TODO add an implementation that tracks recursion depth *)
  (* TODO demo stubbing these out *)
  (* TODO make this use debug.ml *)

  let sep () = " | "
  let fn name = name ^ " <- "
  let arg name value = sprintf "%s = %s" name value
  let result fn_name value = sprintf "%s -> %s" fn_name value

  let print_result fn_name pr_res res =
    print_endline (result fn_name (pr_res res))
  
  let print1 fn_name (a_n, pr_a, a) =
    printf "%s%s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a))])
  let print2 fn_name (a_n, pr_a, a) (b_n, pr_b, b) =
    printf "%s%s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b))])
  let print3 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) =
    printf "%s%s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c))])
  let print4 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) =
    printf "%s%s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d))])
  let print5 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) =
    printf "%s%s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d)); (arg e_n (pr_e e))])
  let print6 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) =
    printf "%s%s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d)); (arg e_n (pr_e e)); (arg f_n (pr_f f))])
  let print7 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) (g_n, pr_g, g) =
    printf "%s%s\n" (fn fn_name) (String.concat (sep ()) [(arg a_n (pr_a a)); (arg b_n (pr_b b)); (arg c_n (pr_c c)); (arg d_n (pr_d d)); (arg e_n (pr_e e)); (arg f_n (pr_f f)); (arg g_n (pr_g g))])
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

  let string_of_tvar name _ =
    match name with
    | "" -> "<polymorphic>"
    | v -> "'" ^ v

  let string_of_option pr x =
    match x with
    | Some s -> "Some " ^ pr s
    | None -> "None"

  let string_of_ref pr x = "ref " ^ pr !x

  let string_of_list pr xs =
    let rec aux xs =
      match xs with
      | [] -> ""
      | [x] -> pr x
      | y :: ys -> pr y ^ "; " ^ aux ys
    in "[" ^ aux xs ^ "]"

  let id x = x

  let string_of_function _ = "<function>"

  let message (m : string) _ = m

  open Printf

  let string_of_tuple pr_a pr_b (a, b) =
    sprintf "(%s, %s)" (pr_a a) (pr_b b)
  let string_of_tuple3 pr_a pr_b pr_c (a, b, c) =
    sprintf "(%s, %s, %s)" (pr_a a) (pr_b b) (pr_c c)
  let string_of_tuple4 pr_a pr_b pr_c pr_d (a, b, c, d) =
    sprintf "(%s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d)
  let string_of_tuple5 pr_a pr_b pr_c pr_d pr_e (a, b, c, d, e) =
    sprintf "(%s, %s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d) (pr_e e)
  let string_of_tuple6 pr_a pr_b pr_c pr_d pr_e pr_f (a, b, c, d, e, f) =
    sprintf "(%s, %s, %s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d) (pr_e e) (pr_f f)
  let string_of_tuple7 pr_a pr_b pr_c pr_d pr_e pr_f pr_g (a, b, c, d, e, f, g) =
    sprintf "(%s, %s, %s, %s, %s, %s, %s)" (pr_a a) (pr_b b) (pr_c c) (pr_d d) (pr_e e) (pr_f f) (pr_g g)
end
