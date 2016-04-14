
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

  class api : object

    (** High-level API, for configuring how tracing is performed. *)

    method sep : string
    method fn : fn_name -> string
    method arg : param_name -> value -> string
    method result : fn_name -> value -> string

    (** Low-level API, for tweaks that fundamentally change how function
        tracing is done. All the different arities need to be implemented
        for consistency.

        There is no conceptual difference between all these functions of
        different arity: a function may be of any arity, depending on how
        many parameters are being tracked. *)

    method print_result : 'a . string -> 'a printer -> 'a -> unit

    method call1 : 'a 'b . loc ->
      ('a -> 'b) -> 'a -> 'b
    method call2 : 'a 'b 'c . loc ->
      ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    method call3 : 'a 'b 'c 'd . loc
      -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    method call4 : 'a 'b 'c 'd 'e . loc
      -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    method call5 : 'a 'b 'c 'd 'e 'f . loc
      -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    method call6 : 'a 'b 'c 'd 'e 'f 'g . loc
      -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    method call7 : 'a 'b 'c 'd 'e 'f 'g 'h . loc
      -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h

    method print_args1 : 'a . string ->
      'a param_spec -> unit
    method print_args2 : 'a 'b . string ->
      'a param_spec ->
      'b param_spec -> unit
    method print_args3 : 'a 'b 'c . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec -> unit
    method print_args4 : 'a 'b 'c 'd . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec -> unit
    method print_args5 : 'a 'b 'c 'd 'e . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec ->
      'e param_spec -> unit
    method print_args6 : 'a 'b 'c 'd 'e 'f . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec ->
      'e param_spec ->
      'f param_spec -> unit
    method print_args7 : 'a 'b 'c 'd 'e 'f 'g . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec ->
      'e param_spec ->
      'f param_spec ->
      'g param_spec -> unit

    method run1 : 'a 'b . string ->
      'a param_spec ->
      'b printer -> ('a -> 'b) -> 'b
    method run2 : 'a 'b 'c . string ->
      'a param_spec ->
      'b param_spec ->
      'c printer -> ('a -> 'b -> 'c) -> 'c
    method run3 : 'a 'b 'c 'd . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd printer -> ('a -> 'b -> 'c -> 'd) -> 'd
    method run4 : 'a 'b 'c 'd 'e . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec ->
      'e printer -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e
    method run5 : 'a 'b 'c 'd 'e 'f . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec ->
      'e param_spec ->
      'f printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'f
    method run6 : 'a 'b 'c 'd 'e 'f 'g . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec ->
      'e param_spec ->
      'f param_spec ->
      'g printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'g
    method run7 : 'a 'b 'c 'd 'e 'f 'g 'h . string ->
      'a param_spec ->
      'b param_spec ->
      'c param_spec ->
      'd param_spec ->
      'e param_spec ->
      'f param_spec ->
      'g param_spec ->
      'h printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'h

  end

  val act : api
end

module DefaultTraceConfig : TraceConfig = struct

  open Printf

  class api = object (self)

    method sep = " | "
    method fn name = name ^ " <-"
    method arg name value = sprintf "%s = %s" name value
    method result fn_name value = sprintf "%s -> %s" fn_name value

    method print_result : 'a . string -> 'a printer -> 'a -> unit =
      fun fn_name pr_res res ->
        print_endline (self#result fn_name (pr_res res))

    method call1 : 'a 'b . loc -> ('a -> 'b) -> 'a -> 'b =
      fun _ fn a -> fn a
    method call2 : 'a 'b 'c . loc -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c =
      fun _ fn a b -> fn a b
    method call3 : 'a 'b 'c 'd . loc -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd =
      fun _ fn a b c -> fn a b c
    method call4 : 'a 'b 'c 'd 'e . loc -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e =
      fun _ fn a b c d -> fn a b c d
    method call5 : 'a 'b 'c 'd 'e 'f . loc -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f =
      fun _ fn a b c d e -> fn a b c d e
    method call6 : 'a 'b 'c 'd 'e 'f 'g . loc -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g =
      fun _ fn a b c d e f -> fn a b c d e f
    method call7 : 'a 'b 'c 'd 'e 'f 'g 'h . loc -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h =
      fun _ fn a b c d e f g -> fn a b c d e f g

    method print_args1 : 'a . string -> 'a param_spec -> unit =
      fun fn_name (a_n, pr_a, a) ->
        printf "%s %s\n" (self#fn fn_name) (String.concat self#sep [(self#arg a_n (pr_a a))])

    method print_args2 : 'a 'b . string -> 'a param_spec -> 'b param_spec -> unit =
      fun fn_name (a_n, pr_a, a) (b_n, pr_b, b) ->
        printf "%s %s\n" (self#fn fn_name) (String.concat self#sep [(self#arg a_n (pr_a a)); (self#arg b_n (pr_b b))])

    method print_args3 : 'a 'b 'c . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> unit =
      fun fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) ->
        printf "%s %s\n" (self#fn fn_name) (String.concat self#sep [(self#arg a_n (pr_a a)); (self#arg b_n (pr_b b)); (self#arg c_n (pr_c c))])

    method print_args4 : 'a 'b 'c 'd . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> unit =
      fun fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) ->
        printf "%s %s\n" (self#fn fn_name) (String.concat self#sep [(self#arg a_n (pr_a a)); (self#arg b_n (pr_b b)); (self#arg c_n (pr_c c)); (self#arg d_n (pr_d d))])

    method print_args5 : 'a 'b 'c 'd 'e . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> 'e param_spec -> unit =
      fun fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) ->
        printf "%s %s\n" (self#fn fn_name) (String.concat self#sep [(self#arg a_n (pr_a a)); (self#arg b_n (pr_b b)); (self#arg c_n (pr_c c)); (self#arg d_n (pr_d d)); (self#arg e_n (pr_e e))])

    method print_args6 : 'a 'b 'c 'd 'e 'f . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> 'e param_spec -> 'f param_spec -> unit =
      fun fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) ->
        printf "%s %s\n" (self#fn fn_name) (String.concat self#sep [(self#arg a_n (pr_a a)); (self#arg b_n (pr_b b)); (self#arg c_n (pr_c c)); (self#arg d_n (pr_d d)); (self#arg e_n (pr_e e)); (self#arg f_n (pr_f f))])

    method print_args7 : 'a 'b 'c 'd 'e 'f 'g . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> 'e param_spec -> 'f param_spec -> 'g param_spec -> unit =
      fun fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) (g_n, pr_g, g) ->
        printf "%s %s\n" (self#fn fn_name) (String.concat self#sep [(self#arg a_n (pr_a a)); (self#arg b_n (pr_b b)); (self#arg c_n (pr_c c)); (self#arg d_n (pr_d d)); (self#arg e_n (pr_e e)); (self#arg f_n (pr_f f)); (self#arg g_n (pr_g g))])

    method run1 : 'a 'b . string -> 'a param_spec -> 'b printer -> ('a -> 'b) -> 'b =
      fun fn_name ((_, _, a) as aa) pr_res fn ->
        self#print_args1 fn_name aa;
        let res = fn a in
        self#print_result fn_name pr_res res;
        res

    method run2 : 'a 'b 'c . string -> 'a param_spec -> 'b param_spec -> 'c printer -> ('a -> 'b -> 'c) -> 'c =
      fun fn_name ((_, _, a) as aa) ((_, _, b) as bb) pr_res fn ->
        self#print_args2 fn_name aa bb;
        let res = fn a b in
        self#print_result fn_name pr_res res;
        res

    method run3 : 'a 'b 'c 'd . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd printer -> ('a -> 'b -> 'c -> 'd) -> 'd =
      fun fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc) pr_res fn ->
        self#print_args3 fn_name aa bb cc;
        let res = fn a b c in
        self#print_result fn_name pr_res res;
        res

    method run4 : 'a 'b 'c 'd 'e . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> 'e printer -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e =
      fun fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc) ((_, _, d) as dd) pr_res fn ->
        self#print_args4 fn_name aa bb cc dd;
        let res = fn a b c d in
        self#print_result fn_name pr_res res;
        res

    method run5 : 'a 'b 'c 'd 'e 'f . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> 'e param_spec -> 'f printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'f =
      fun fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc) ((_, _, d) as dd) ((_, _, e) as ee) pr_res fn ->
        self#print_args5 fn_name aa bb cc dd ee;
        let res = fn a b c d e in
        self#print_result fn_name pr_res res;
        res

    method run6 : 'a 'b 'c 'd 'e 'f 'g . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> 'e param_spec -> 'f param_spec -> 'g printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'g =
      fun fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc) ((_, _, d) as dd) ((_, _, e) as ee) ((_, _, f) as ff) pr_res fn ->
        self#print_args6 fn_name aa bb cc dd ee ff;
        let res = fn a b c d e f in
        self#print_result fn_name pr_res res;
        res

    method run7 : 'a 'b 'c 'd 'e 'f 'g 'h . string -> 'a param_spec -> 'b param_spec -> 'c param_spec -> 'd param_spec -> 'e param_spec -> 'f param_spec -> 'g param_spec -> 'h printer -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'h =
      fun fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc) ((_, _, d) as dd) ((_, _, e) as ee) ((_, _, f) as ff) ((_, _, g) as gg) pr_res fn ->
        self#print_args7 fn_name aa bb cc dd ee ff gg;
        let res = fn a b c d e f g in
        self#print_result fn_name pr_res res;
        res
  end

  let act = new api
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
    | v -> fprintf fmt "'_%s" v

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

type ('a, 'b) traced1 = Traced1 of ('a -> 'b)
type ('a, 'b, 'c) traced2 = Traced2 of ('a -> 'b -> 'c)
type ('a, 'b, 'c, 'd) traced3 = Traced3 of ('a -> 'b -> 'c -> 'd)
type ('a, 'b, 'c, 'd, 'e) traced4 = Traced4 of ('a -> 'b -> 'c -> 'd -> 'e)
type ('a, 'b, 'c, 'd, 'e, 'f) traced5 = Traced5 of ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
type ('a, 'b, 'c, 'd, 'e, 'f, 'g) traced6 = Traced6 of ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) traced7 = Traced7 of ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h)

let wrap1 loc (module TC : TraceConfig) (Traced1 fn) a = TC.act#call1 loc fn a
let wrap2 loc (module TC : TraceConfig) (Traced2 fn) a b = TC.act#call2 loc fn a b
let wrap3 loc (module TC : TraceConfig) (Traced3 fn) a b c = TC.act#call3 loc fn a b c
let wrap4 loc (module TC : TraceConfig) (Traced4 fn) a b c d = TC.act#call4 loc fn a b c d
let wrap5 loc (module TC : TraceConfig) (Traced5 fn) a b c d e = TC.act#call5 loc fn a b c d e
let wrap6 loc (module TC : TraceConfig) (Traced6 fn) a b c d e f = TC.act#call6 loc fn a b c d e f
let wrap7 loc (module TC : TraceConfig) (Traced7 fn) a b c d e f g = TC.act#call7 loc fn a b c d e f g
