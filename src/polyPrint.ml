
let to_string : 'a -> string =
  fun _ -> raise (Failure "ppx_polyprint not set up properly?")

let string_of = to_string

let print x = x |> to_string |> print_endline

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
