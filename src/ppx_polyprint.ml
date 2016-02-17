
open Ast_mapper
open Ast_helper
open Asttypes
open Longident

let composed_pre_mapper =
  { default_mapper with
    expr = begin
      fun mapper expr ->
        expr
        |> PolyPrintLog.mapper.expr PolyPrintLog.mapper
      (* |> (fun e -> print_endline @@ Pprintast.string_of_expression e; e) *)
        |> PolyPrintShow.eta_expansion_mapper.expr PolyPrintShow.eta_expansion_mapper
      (* |> (fun e -> print_endline @@ Pprintast.string_of_expression e; e) *)
    end;
    structure_item =
      fun mapper expr ->
        expr
        |> PolyPrintLog.mapper.structure_item PolyPrintLog.mapper
      (* |> (fun e -> print_endline @@ Pprintast.string_of_structure [e]; e) *)
        |> PolyPrintShow.eta_expansion_mapper.structure_item PolyPrintShow.eta_expansion_mapper
      (* |> (fun e -> print_endline @@ Pprintast.string_of_structure [e]; e) *)
  }

module Map = TypedtreeMap.MakeMap(PolyPrintShow.MapArg)

module TypedIdentityTransformation = struct
  let map_structure x = x
  let map_signature x = x
end

module Main = Typpx.Make.F(struct
    let tool_name = "ppx_polyprint"
    let args = []
    let firstUntypedTransformation = composed_pre_mapper
    module Typemod = Typpx.Default.Typemod
    module TypedTransformation = Map
    let lastUntypedTransformation = Typpx.Default.untyped_identity
  end)

let () = Main.run ()

(* let () = register "ppx_polyprint" (fun _argv -> default_mapper) *)
