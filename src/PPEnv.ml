(** Information passed across traversals *)

(** Names of annotated functions that were successfully transformed.Built up
    during AST traversal. Stateful, and thus mappers here are not reentrant. *)
let transformed_function_names = ref ([] : string list)

(** Configuration module path provided for each function name *)
module NameConfigMap = Map.Make(String)
let configuration_modules = ref (NameConfigMap.empty : string list NameConfigMap.t)

(** The default module to use if not explicitly given *)
let specified_default_module = ref (None : string list option)
