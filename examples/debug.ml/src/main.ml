
open Config

let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)
  [@@tracerec]

let plus a b = a + b
  [@@trace Adapter; a]

let () =
  Arg.parse Debug.command_args (fun _ -> ()) "";
  ignore @@ fact 5;
  ignore @@ plus 2 5;
