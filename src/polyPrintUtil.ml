
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

module Names = struct
  let runtime = "PolyPrint"
  let printers = "Printers"
  let default_log = "Default"
  let to_string = ["to_string"; "string_of"; "show"]
  let print = "print"
  let print_n n = "print" ^ string_of_int n
end

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

let pat_var name =
  { ppat_desc = Ppat_var { txt = name; loc = dummy_loc };
    ppat_loc = dummy_loc;
    ppat_attributes = []
  }

let binding pat expr =
  { pvb_pat = pat;
    pvb_expr = expr;
    pvb_loc = dummy_loc;
    pvb_attributes = []
  }
