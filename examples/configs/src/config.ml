
[@@@polyprint Recursive]

module Recursive : PolyPrint.TraceConfig = struct
  include PolyPrint.DefaultTraceConfig

  let depth = ref 0

  let spaces n =
    let rec aux bar n =
      match n with
      | 0 -> ""
      | n -> (if bar then "| " else " ") ^ aux (not bar) (n - 1)
    in aux true n

  let incr_depth () =
    spaces !depth |> print_string;
    incr depth

  let decr_depth () =
    decr depth;
    spaces !depth |> print_string

  let print_result fn_name pr_res res =
    decr_depth ();
    PolyPrint.DefaultTraceConfig.print_result fn_name pr_res res

  let run1 fn_name ((_, _, a) as aa) pr_res fn =
    incr_depth ();
    print_args1 fn_name aa;
    let res = fn a in
    print_result fn_name pr_res res;
    res

  let run2 fn_name ((_, _, a) as aa) ((_, _, b) as bb) pr_res fn =
    incr_depth ();
    print_args2 fn_name aa bb;
    let res = fn a b in
    print_result fn_name pr_res res;
    res

  let run3 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc) pr_res fn =
    incr_depth ();
    print_args3 fn_name aa bb cc;
    let res = fn a b c in
    print_result fn_name pr_res res;
    res

  let run4 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) pr_res fn =
    incr_depth ();
    print_args4 fn_name aa bb cc dd;
    let res = fn a b c d in
    print_result fn_name pr_res res;
    res

  let run5 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) ((_, _, e) as ee) pr_res fn =
    incr_depth ();
    print_args5 fn_name aa bb cc dd ee;
    let res = fn a b c d e in
    print_result fn_name pr_res res;
    res

  let run6 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) ((_, _, e) as ee) ((_, _, f) as ff) pr_res fn =
    incr_depth ();
    print_args6 fn_name aa bb cc dd ee ff;
    let res = fn a b c d e f in
    print_result fn_name pr_res res;
    res

  let run7 fn_name ((_, _, a) as aa) ((_, _, b) as bb) ((_, _, c) as cc)
      ((_, _, d) as dd) ((_, _, e) as ee) ((_, _, f) as ff)
      ((_, _, g) as gg) pr_res fn =
    incr_depth ();
    print_args7 fn_name aa bb cc dd ee ff gg;
    let res = fn a b c d e f g in
    print_result fn_name pr_res res;
    res
end

