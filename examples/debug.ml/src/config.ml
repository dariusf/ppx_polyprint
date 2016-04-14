
module Adapter : PolyPrint.TraceConfig = struct

  class api = object (self)
    inherit PolyPrint.DefaultTraceConfig.api as super

    method run1 fn_name (a_n, pr_a, a) pr_res fn =
      Debug.no_1 fn_name pr_a pr_res fn a

    method run2 fn_name (a_n, pr_a, a) (b_n, pr_b, b) pr_res fn =
      Debug.no_2 fn_name pr_a pr_b pr_res fn a b

    method run3 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) pr_res fn =
      Debug.no_3 fn_name pr_a pr_b pr_c pr_res fn a b c

    method run4 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) pr_res fn =
      Debug.no_4 fn_name pr_a pr_b pr_c pr_d pr_res fn a b c d

    method run5 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) pr_res fn =
      Debug.no_5 fn_name pr_a pr_b pr_c pr_d pr_e pr_res fn a b c d e

    method run6 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) pr_res fn =
      Debug.no_6 fn_name pr_a pr_b pr_c pr_d pr_e pr_f pr_res fn a b c d e f

    method run7 fn_name (a_n, pr_a, a) (b_n, pr_b, b) (c_n, pr_c, c) (d_n, pr_d, d) (e_n, pr_e, e) (f_n, pr_f, f) (g_n, pr_g, g) pr_res fn =
      Debug.no_7 fn_name pr_a pr_b pr_c pr_d pr_e pr_f pr_g pr_res fn a b c d e f g

    method call1 (file, line) fn a =
      Debug.wrap_z_debug VarGen.store_loc_str file line;
      fn a

    method call2 (file, line) fn a b =
      Debug.wrap_z_debug VarGen.store_loc_str file line;
      fn a b

    method call3 (file, line) fn a b c =
      Debug.wrap_z_debug VarGen.store_loc_str file line;
      fn a b c

    method call4 (file, line) fn a b c d =
      Debug.wrap_z_debug VarGen.store_loc_str file line;
      fn a b c d

    method call5 (file, line) fn a b c d e =
      Debug.wrap_z_debug VarGen.store_loc_str file line;
      fn a b c d e

    method call6 (file, line) fn a b c d e f =
      Debug.wrap_z_debug VarGen.store_loc_str file line;
      fn a b c d e f

    method call7 (file, line) fn a b c d e f g =
      Debug.wrap_z_debug VarGen.store_loc_str file line;
      fn a b c d e f g
  end

  let act = new api
end
