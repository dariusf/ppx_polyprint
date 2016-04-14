
module Recursive : PolyPrint.TraceConfig = struct

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

  class api = object (self)
    inherit PolyPrint.DefaultTraceConfig.api as super

    method print_result fn_name pr_res res =
      decr_depth ();
      super#print_result fn_name pr_res res

    method run1 fn_name a pr_res fn =
      incr_depth ();
      super#run1 fn_name a pr_res fn

    method run2 fn_name a b pr_res fn =
      incr_depth ();
      super#run2 fn_name a b pr_res fn

    method run3 fn_name a b c pr_res fn =
      incr_depth ();
      super#run3 fn_name a b c pr_res fn

    method run4 fn_name a b c d pr_res fn =
      incr_depth ();
      super#run4 fn_name a b c d pr_res fn

    method run5 fn_name a b c d e pr_res fn =
      incr_depth ();
      super#run5 fn_name a b c d e pr_res fn

    method run6 fn_name a b c d e f pr_res fn =
      incr_depth ();
      super#run6 fn_name a b c d e f pr_res fn

    method run7 fn_name a b c d e f g pr_res fn =
      incr_depth ();
      super#run7 fn_name a b c d e f g pr_res fn
  end

  let act = new api
end

module Minimal : PolyPrint.TraceConfig = struct
  class api = object (self)
    inherit Recursive.api

    method fn name = name ^ ""
    method arg _ value = value
    method result _ value = "= " ^ value
  end

  let act = new api
end
