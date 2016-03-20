
open Yojson.Basic
open Sys

let filename = "polyprint.log"

let read () =
  if file_exists filename then
    from_file filename |> PPEnv.from_json

let write () =
  PPEnv.to_json () |> to_file filename

