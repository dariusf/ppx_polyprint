
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

let rec fib n =
  if n <= 1 then 1 else fib (n - 1) + fib (n - 2)
  [@@tracerec Minimal]

let () =
  roll 5;
  ignore @@ fib 5
