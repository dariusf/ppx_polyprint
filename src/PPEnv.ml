
(** Information passed across traversals *)

(** Configuration module path provided for each function name *)
module NameConfigMap = Map.Make(String)
let configuration_modules = ref (NameConfigMap.empty : string list NameConfigMap.t)

(** The default module to use if not explicitly given *)
let specified_default_module = ref (None : string list option)
