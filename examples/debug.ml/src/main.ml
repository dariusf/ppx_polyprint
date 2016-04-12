
open Config
open Other

[@@@polyprint Adapter]

let plus a b = a + b
  [@@trace Adapter; a]

let () =
  Arg.parse Debug.command_args (fun _ -> ()) "";
  ignore @@ fact 5;
  ignore @@ plus 2 5;
