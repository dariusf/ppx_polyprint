
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

let dummy_loc =
  {
    Location.loc_start = Lexing.dummy_pos;
    Location.loc_end = Lexing.dummy_pos;
    Location.loc_ghost = true
  }

let app_mapper find replace =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc =
            Pexp_apply
              ({ pexp_desc = Pexp_ident { txt = Lident fn_name; loc } }, args) }
        when fn_name = find ->
        Exp.apply (Exp.ident { txt=Lident replace; loc }) args
      | _ -> default_mapper.expr mapper expr
  }
