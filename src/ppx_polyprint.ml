
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
        |> PolyPrintShow.eta_expansion_mapper.expr PolyPrintShow.eta_expansion_mapper
    end;
    structure_item = PolyPrintLog.mapper.structure_item
  }

module Map = TypedtreeMap.MakeMap(PolyPrintShow.MapArg)

module Main = Typpx.Make.F(struct
    let tool_name = "ppx_polyprint"
    let args = []
    let firstUntypedTransformation = composed_pre_mapper
    module Typemod = Typpx.Default.Typemod
    module TypedTransformation = Map
    let lastUntypedTransformation = Typpx.Default.untyped_identity
  end)

let () = Main.run ()
