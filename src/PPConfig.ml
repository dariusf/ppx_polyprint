
open Asttypes
open Longident

open PPUtil
open PPUtil.Untyped
open Parsetree

type t = {
  module_prefix : string list;
  vars : (string) list
}

let rec list_of_sequence seq =
  match seq with
  | { pexp_desc = Pexp_sequence (e, f) } -> e :: list_of_sequence f
  | _ -> [seq]

let default_config = {
  module_prefix = [Names.runtime; Names.default_module];
  vars = [];
}

(** Indirection for getting the default module in the face of stateful
    configuration variables. Should be removed when this extension can be
    parameterised by it *)
let default_module () =
  otherwise default_config.module_prefix !PPEnv.specified_default_module

let rec interpret_one e config =
  match e with
  | { pexp_desc = Pexp_construct ({ txt = Lident name }, None) } ->
      (* a module *)
      { config with module_prefix = [name] }
  | { pexp_desc = Pexp_construct ({ txt = path }, None) } ->
      (* a qualified module *)
      { config with module_prefix = longident_to_list path }
  | { pexp_desc = Pexp_tuple ts } ->
      (* tuples are interchangeable with sequences for the most part,
         but sequences may not be nested inside them *)
      List.fold_right interpret_one ts config
  | { pexp_desc = Pexp_ident { txt = Lident name } } ->
      (* a variable name *)
      { config with vars = name :: config.vars }
  | _ -> config

let interpret attrs =
  match attrs with
  | [] -> { default_config with module_prefix = default_module () }
  | x :: _ -> (* TODO consider all, not just first *)
      begin match x with
      | _, PStr [{pstr_desc = Pstr_eval (seq, _)}] ->
          let config_fields = list_of_sequence seq in
          List.fold_right interpret_one config_fields
            { default_config with module_prefix = default_module () }
      | _ -> { default_config with module_prefix = default_module () }
      end
