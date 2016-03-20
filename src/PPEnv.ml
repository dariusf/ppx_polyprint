
(** Information passed across traversals *)

(** Configuration module path provided for each function name *)
module NameConfigMap = Map.Make(String)
let configuration_modules = ref (NameConfigMap.empty : string list NameConfigMap.t)

(** The default module to use if not explicitly given *)
let specified_default_module = ref (None : string list option)

let init () =
  configuration_modules := NameConfigMap.empty;
  specified_default_module := None

(** Names of annotated functions that were successfully transformed.Built up
    during AST traversal. Stateful, and thus mappers here are not reentrant. *)
let transformed_function_names () =
  NameConfigMap.bindings !configuration_modules
  |> List.map fst

module Deserialise = struct

  exception Expected_something_else

  let to_strings =
    let transform = function
      | `String s -> s
      | _ -> raise Expected_something_else
    in
    List.map transform
  
  let analyse_function_configs (name, value) =
    let fail () = failwith @@ "did not get a module name for function " ^ name in
    match value with
    | `List module_name ->
        begin
          try
            configuration_modules :=
              NameConfigMap.add name (to_strings module_name) !configuration_modules
          with Expected_something_else -> fail()
        end
    | _ -> fail()

  let process_key (s, content) =
    match s with
    | "default" ->
        let fail () = failwith "default expects a list of strings representing a module path" in
        begin match content with
        | `List ss ->
            begin
              try
                specified_default_module := Some (ss |> to_strings)
              with Expected_something_else -> fail ()
            end
        | _ -> fail ()
        end
    | "functions" ->
        begin match content with
        | `Assoc fn_configs ->
            List.iter analyse_function_configs fn_configs
        | _ -> failwith "functions expects a map of functions"
        end
    | _ -> failwith "unexpected top-level key"

  let from_json (json:Yojson.Basic.json) =
    match json with
    | `Assoc tlkeys -> List.iter process_key tlkeys
    | _ -> failwith "not a json object"
end

module Serialise = struct

  let of_strings s =
    `List (List.map (fun x -> `String x) s)

  let of_map m =
    NameConfigMap.bindings m
    |> List.map (fun (k, v) -> (k, of_strings v))
    |> (fun x -> `Assoc x)

  let to_json () =
    let default_key =
      match !specified_default_module with
      | Some s -> ["default", of_strings s]
      | None -> []
    in
    let function_key =
      match NameConfigMap.is_empty !configuration_modules with
      | true -> []
      | _ -> ["functions", of_map !configuration_modules ]
    in
    `Assoc (default_key @ function_key)
end

let from_json s =
  try
    ignore @@ Deserialise.from_json s
  with Failure _ ->
    init ()

let to_json = Serialise.to_json
