

#define hi hello
#define x_add_0 (fun f -> let () = Debug.wrap_z_debug VarGen.store_loc_str  __FILE__ __LINE__ in f)
#define x_add_1 (fun f x -> let () = Debug.wrap_z_debug VarGen.store_loc_str  __FILE__ __LINE__ in f x)
#define x_add (fun f x y -> let () = Debug.wrap_z_debug VarGen.store_loc_str  __FILE__ __LINE__ in f x y)
#define x_add_3 (fun f a b c -> let () = Debug.wrap_z_debug VarGen.store_loc_str  __FILE__ __LINE__ in f a b c)

let hello x y z = print_endline (x ^ y ^ z)

let test f a b = f a b

let () =
  hi "a" "a" "a";
  test hello "a" "b" "c";

(* let rec fact n = *)
(*   let fact_original self n = if n = 0 then 1 else n * (self (n - 1)) in *)
(*  let rec aux n = *)
(*     Debug.no_1 "fact" string_of_int *)
(*       string_of_int (fact_original fact) n in *)
(*   aux n *)

open PolyPrint

module Custom : TraceConfig = struct
  include DefaultTraceConfig

  let run1 fn_name (a_n, pr_a, a) pr_res fn =
    Debug.no_1 fn_name pr_a pr_res fn a

  let run2 fn_name (a_n, pr_a, a) (b_n, pr_b, b) pr_res fn =
    Debug.no_2 fn_name pr_a pr_b pr_res fn a b

  let run3 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) pr_res fn =
    Debug.no_3 fn_name pr_a pr_b pr_c pr_res fn a b c

  let run4 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) pr_res fn =
    Debug.no_4 fn_name pr_a pr_b pr_c pr_d pr_res fn a b c d

  let run5 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) pr_res fn =
    Debug.no_5 fn_name pr_a pr_b pr_c pr_d pr_e pr_res fn a b c d e

  let run6 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) pr_res fn =
    Debug.no_6 fn_name pr_a pr_b pr_c pr_d pr_e pr_f pr_res fn a b c d e f

  let run7 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) (g_n, pr_g, g) pr_res fn =
    Debug.no_7 fn_name pr_a pr_b pr_c pr_d pr_e pr_f pr_g pr_res fn a b c d e f g

  let call1 (file, line) fn a =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a

  let call2 (file, line) fn a b =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b

  let call3 (file, line) fn a b c =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c

  let call4 (file, line) fn a b c d =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d

  let call5 (file, line) fn a b c d e =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d e

  let call6 (file, line) fn a b c d e f =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d e f

  let call7 (file, line) fn a b c d e f g =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d e f g
end


(* let rec fact n = *)
  (* if n = 0 then 1 else n * fact (n - 1) *)
  (* [@@trace] *)

let () =
  let [@trace ] rec fact n =
    if n = 0 then 1 else n * fact (n - 1)
  in
  let [@trace Custom; a; b] plus a b = a + b
  in
  Arg.parse Debug.command_args (fun s  -> ()) "";
  (* ignore @@ (1 + x_add_0 fact 5); *)
  (* ignore @@ (1 + x_add_1 plus 2 5) *)
  ignore @@ (1 + fact 5);
  ignore @@ (1 + plus 2 5)
