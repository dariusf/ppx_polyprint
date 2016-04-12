
(** Information passed across traversals *)

(** Configuration module path provided for each function name *)
module NameConfigMap = Map.Make(String)
let configuration_modules = ref (NameConfigMap.empty : string list NameConfigMap.t)

(** The default module to use if not explicitly given *)
let specified_default_module = ref (None : string list option)

let debug_mode = ref false

let check_debug_mode () =
  try
    ignore @@ Sys.getenv "POLYPRINT_DEBUG";
    debug_mode := true
  with Not_found -> ()

let init () =
  check_debug_mode ()
