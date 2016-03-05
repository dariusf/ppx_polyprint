
open PolyPrint

type flip = Flip | Flop
  [@@deriving show]

let switch = function
  | Flip -> Flop
  | Flop -> Flip

let rec fact n x =
  if n = 0 then 1
  else n * fact (n - 1) (switch x)
  [@@tracerec]

and plus x y = x + y
  [@@trace]

let () =
  ignore @@ fact 5 Flip;
  ignore @@ plus 2 3;
