
open Ast_mapper
open Asttypes

(** Code in this module is taken from Typpx's `make` and `compile`
    modules. It depends on the internals of both ocamlc and Typpx and
    should be considered fragile across different versions of both. *)

module Compile = struct
(*
  This is a modified version of driver/compile.ml of OCaml.
*)
  [@@@ocaml.warning "-27"]

  (* The batch compiler *)

  open Format
  open Typedtree
  open Compenv

  module Make(Typemod : Typpx.S.Typemod)(TypedTransformation : Typpx.S.TypedTransformation) = struct

    (* Compile a .mli file *)

    (* Keep in sync with the copy in optcompile.ml *)

    let tool_name = "ocamlc"

    let interface ppf sourcefile outputprefix ast =
      Compmisc.init_path false;
      let modulename = module_of_filename ppf sourcefile outputprefix in
      Env.set_unit_name modulename;
      let initial_env = Compmisc.initial_env () in
      if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
      if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;
      let tsg = Typemod.type_interface initial_env ast in
      if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
      let sg = tsg.sig_type in
      if !Clflags.print_types then
        Printtyp.wrap_printing_env initial_env (fun () ->
            fprintf std_formatter "%a@."
              Printtyp.signature (Typemod.simplify_signature sg));
      ignore (Includemod.signatures initial_env sg sg);
      Typecore.force_delayed_checks ();
      Warnings.check_fatal ();
      tsg

    (* Compile a .ml file *)

    let print_if ppf flag printer arg =
      if !flag then fprintf ppf "%a@." printer arg;
      arg

    let (++) x f = f x

    let implementation ppf sourcefile outputprefix ast =
      Compmisc.init_path false;
      let modulename = module_of_filename ppf sourcefile outputprefix in
      Env.set_unit_name modulename;
      let env = Compmisc.initial_env() in
      try
        let (typedtree, coercion) =
          ast
          ++ print_if ppf Clflags.dump_parsetree Printast.implementation
          ++ print_if ppf Clflags.dump_source Pprintast.structure
          ++ Typemod.type_implementation sourcefile outputprefix modulename env
          ++ print_if ppf Clflags.dump_typedtree
            Printtyped.implementation_with_coercion
        in
        if !Clflags.print_types then begin
          Warnings.check_fatal ();
          Stypes.dump (Some (outputprefix ^ ".annot"));
        end else begin
          Stypes.dump (Some (outputprefix ^ ".annot"));
        end;
        typedtree
      with x ->
        Stypes.dump (Some (outputprefix ^ ".annot"));
        raise x
  end
end

module Make = struct

  module Embed = struct
    (* e [@embed e'] => e' *)

    open Parsetree

    let extend super =
      let expr self e = match e.pexp_attributes with
        | [ {txt="typpx_embed"}, PStr [ { pstr_desc= Pstr_eval (e, []) } ] ] -> e
        | _ -> super.expr self e
      in
      { super with expr }

    let mapper = extend Ast_mapper.default_mapper

  end

  module F(A : sig
      val tool_name : string
      val args : (Arg.key * Arg.spec * Arg.doc) list
      val firstUntypedTransformation : mapper
      module Typemod : Typpx.S.Typemod
      module TypedTransformation : Typpx.S.TypedTransformation
      val lastUntypedTransformation : mapper
    end) = struct

    open A

    module Typecheck = Compile.Make(Typemod)(TypedTransformation)

    let dump_first = ref None
    let dump_untype = ref None

    let dump path ast = match path with
      | None -> ()
      | Some path ->
          let oc = open_out path in
          let ppf = Format.formatter_of_out_channel oc in
          begin match ast with
          | `Str str -> Pprintast.structure ppf str
          | `Sig sg -> Pprintast.signature ppf sg
          end;
          Format.pp_print_newline ppf ();
          close_out oc

    let dump_str path str = dump path (`Str str); str
    let dump_sig path sg  = dump path (`Sig sg);  sg

    let rev_ppxs = ref []
    let add_ppx s = rev_ppxs := s :: !rev_ppxs

    (* The PPX mapper *)

    let mapper = match Ast_mapper.tool_name () with
      | "ocamldep" ->
          (* If the tool is ocamldep, we CANNOT type-check *)
          firstUntypedTransformation
      | tool_name ->
          Clflags.all_ppx := List.rev !rev_ppxs;
          let structure _x str =
            Clflags.dont_write_files := true;
            Warnings.parse_options false "a"; (* print warning *)
            Warnings.parse_options true  "a"; (* warning as error *)
            firstUntypedTransformation.structure firstUntypedTransformation str
            |> dump_str !dump_first
            |> Typecheck.implementation Format.err_formatter "papa" (* dummy *) "gaga" (* dummy *)
            |> TypedTransformation.map_structure
            |> Typpx.Untypeast.untype_structure
            |> dump_str !dump_untype
            |> Embed.mapper.structure Embed.mapper
            |> lastUntypedTransformation.structure lastUntypedTransformation
            |> Pparse.apply_rewriters_str ~tool_name
          in
          let signature _x sg =
            Clflags.dont_write_files := true;
            Warnings.parse_options false "a"; (* print warning *)
            Warnings.parse_options true  "a"; (* warning as error *)
            firstUntypedTransformation.signature firstUntypedTransformation sg
            |> dump_sig !dump_first
            |> Typecheck.interface Format.err_formatter "papa" (* dummy *) "gaga" (* dummy *)
            |> TypedTransformation.map_signature
            |> Typpx.Untypeast.untype_signature
            |> dump_sig !dump_untype
            |> Embed.mapper.signature Embed.mapper
            |> lastUntypedTransformation.signature lastUntypedTransformation
            |> Pparse.apply_rewriters_sig ~tool_name
          in
          { default_mapper with structure; signature }

    let opts =
      let set_string_opt r = Arg.String (fun s -> r := Some s) in
      [( "-typpx-dump-first", set_string_opt dump_first, "<path>: (TyPPX) Dump the result of the first untyped transformation stage" );
       ( "-typpx-dump-untype", set_string_opt dump_untype, "<path>: (TyPPX) Dump the result of the untype stage" );
       ( "-ppx", Arg.String add_ppx, "<command>: (TyPPX) Run extra PPX preprocessing at the final phase of TyPPX" );
      ]

    let run () = Ppxx.Ppx.run (args @ opts) tool_name (fun () -> ()) mapper
  end
end
