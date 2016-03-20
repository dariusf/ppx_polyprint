
[@@@polyprint Adapter]

module Adapter : PolyPrint.TraceConfig = struct
  include PolyPrint.DefaultTraceConfig

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

  let call1 _ (file, line) fn a =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a

  let call2 _ (file, line) fn a b =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b

  let call3 _ (file, line) fn a b c =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c

  let call4 _ (file, line) fn a b c d =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d

  let call5 _ (file, line) fn a b c d e =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d e

  let call6 _ (file, line) fn a b c d e f =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d e f

  let call7 _ (file, line) fn a b c d e f g =
    Debug.wrap_z_debug VarGen.store_loc_str file line;
    fn a b c d e f g
end
