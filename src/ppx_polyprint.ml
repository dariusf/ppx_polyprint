
open Ast_mapper
open Ast_helper
open Asttypes
open Longident

let pre_mapper =
  { default_mapper with
    expr = begin
      fun mapper expr ->
        expr
        |> PPTrace.annotation_mapper.expr
          PPTrace.annotation_mapper
        (* |> (fun e -> print_endline @@ Pprintast.string_of_expression e; e) *)
        |> PPShow.eta_abstraction_mapper.expr
          PPShow.eta_abstraction_mapper
          (* |> (fun e -> print_endline @@ Pprintast.string_of_expression e; e) *)
    end;
    structure_item =
      fun mapper expr ->
        expr
        |> PPTrace.annotation_mapper.structure_item
          PPTrace.annotation_mapper
        (* |> (fun e -> print_endline @@ Pprintast.string_of_structure [e]; e) *)
        |> PPShow.eta_abstraction_mapper.structure_item
          PPShow.eta_abstraction_mapper
          (* |> (fun e -> print_endline @@ Pprintast.string_of_structure [e]; e) *)
  }

module Main = PPDriver.Make.F(struct
    let tool_name = "ppx_polyprint"
    let args = []
    let firstUntypedTransformation = pre_mapper
    module Typemod = PPTypemod
    module TypedTransformation = TypedtreeMap.MakeMap(PPShow.TypedTransform)
    let lastUntypedTransformation = Typpx.Default.untyped_identity
  end)

let () =
  PPEnv.init ();
  Main.run ();
