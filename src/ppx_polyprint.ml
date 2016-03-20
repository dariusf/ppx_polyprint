
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
        |> PPShow.eta_expansion_mapper.expr
          PPShow.eta_expansion_mapper
          (* |> (fun e -> print_endline @@ Pprintast.string_of_expression e; e) *)
    end;
    structure_item =
      fun mapper expr ->
        expr
        |> PPTrace.annotation_mapper.structure_item
          PPTrace.annotation_mapper
        (* |> (fun e -> print_endline @@ Pprintast.string_of_structure [e]; e) *)
        |> PPShow.eta_expansion_mapper.structure_item
          PPShow.eta_expansion_mapper
          (* |> (fun e -> print_endline @@ Pprintast.string_of_structure [e]; e) *)
  }

let post_mapper =
  { default_mapper with
    expr = begin
      fun mapper expr ->
        expr
        |> PPTrace.call_wrapping_mapper.expr
          PPTrace.call_wrapping_mapper
    end;
    structure_item =
      fun mapper expr ->
        expr
        |> PPTrace.call_wrapping_mapper.structure_item
          PPTrace.call_wrapping_mapper
  }

module Main = Typpx.Make.F(struct
    let tool_name = "ppx_polyprint"
    let args = []
    let firstUntypedTransformation = pre_mapper
    module Typemod = Typpx.Default.Typemod
    module TypedTransformation = TypedtreeMap.MakeMap(PPShow.TypedTransform)
    let lastUntypedTransformation = post_mapper
  end)

let () =
  PPAcross.read ();
  Main.run ();
  PPAcross.write ();
