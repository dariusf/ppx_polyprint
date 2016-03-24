
open Config

let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)
  [@@tracerec Recursive]

let rec roll n =
  match n with
  | 0 -> ()
  | 3 -> ignore @@ fact 5; roll 2
  | n -> roll (n - 1)
  [@@tracerec Recursive]

let () =
  roll 5
