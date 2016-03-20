
open Config
open Other

let plus a b = a + b
  [@@trace a]

let () =
  Arg.parse Debug.command_args (fun _ -> ()) "";
  ignore @@ fact 5;
  ignore @@ plus 2 5;
