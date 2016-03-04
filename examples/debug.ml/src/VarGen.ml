let dummy = ()


type print_level =
  (* | P_Quiet *)
  (* | P_VShort *)
  | P_Short
  | P_Norm  (* default *)
  | P_Detail
  (* | P_Debug *)

type print_set =
  | PS_Debug (* to assist with debugging *)
  | PS_Type (* to print type *)
  | PS_Quiet (* quiet printing *)
  | PS_Orig_Conseq (* quiet printing *)
  | PS_Tidy (* quiet printing *)
  | PS_IParams (* quiet printing *)
  | PS_HTML (* quiet printing *)

let compete_mode = ref false
let trace_failure = ref false
let trace_exc = ref false
let trace_loop = ref false
let trace_loop_all = ref false
let verbose_num = ref 0

let last_posn = ref (None:string option)

let suppress_warning_msg = ref false
let en_warning_msg = ref true

let sap = ref false

type loc =  {
  start_pos : Lexing.position (* might be expanded to contain more information *);
  mid_pos : Lexing.position;
  end_pos : Lexing.position;
}

type primed =
  | Primed
  | Unprimed

let string_of_primed p =
  match p with
  | Primed -> "Primed"
  | Unprimed -> "Unprimed"

let no_pos = 
  let no_pos1 = { Lexing.pos_fname = "";
                  Lexing.pos_lnum = 0;
                  Lexing.pos_bol = 0; 
                  Lexing.pos_cnum = 0 } in
  {start_pos = no_pos1; mid_pos = no_pos1; end_pos = no_pos1;}


let is_no_pos l = (l.start_pos.Lexing.pos_cnum == 0)

let print_endline_q s =
  if !compete_mode then ()
  else print_endline s

let print_backtrace_quiet () =
  if !compete_mode then ()
  else
    Printexc.print_backtrace stdout

let get_backtrace_quiet () =
  if !compete_mode then ""
  else
    Printexc.get_backtrace ()

let record_backtrace_quite () =
  if !compete_mode then ()
  else
    Printexc.record_backtrace !trace_failure

let string_of_loc (p : loc) = 
  p.start_pos.Lexing.pos_fname ^ "_" ^ 
  (string_of_int p.start_pos.Lexing.pos_lnum) ^ ":" ^
  (string_of_int (p.start_pos.Lexing.pos_cnum-p.start_pos.Lexing.pos_bol)) ^ "_" ^
  (string_of_int p.end_pos.Lexing.pos_lnum) ^ ":" ^
  (string_of_int (p.end_pos.Lexing.pos_cnum-p.end_pos.Lexing.pos_bol))

let string_of_pos (p : Lexing.position) = "("^string_of_int(p.Lexing.pos_lnum) ^","^string_of_int(p.Lexing.pos_cnum-p.Lexing.pos_bol) ^")"
;;

let string_of_full_loc (l : loc) = "{"^(string_of_pos l.start_pos)^","^(string_of_pos l.end_pos)^"}";;

let string_of_loc_by_char_num (l : loc) = 
  Printf.sprintf "(%d-%d)"
    l.start_pos.Lexing.pos_cnum
    l.end_pos.Lexing.pos_cnum

let eq_pos p1 p2 = 
  (p1.Lexing.pos_lnum == p2.Lexing.pos_lnum) &&
  (p1.Lexing.pos_cnum - p1.Lexing.pos_bol) == (p2.Lexing.pos_cnum - p2.Lexing.pos_bol)

let eq_loc l1 l2 = 
  eq_pos l1.start_pos l2.start_pos

(*Proof logging facilities*)
class ['a] store (x_init:'a) (epr:'a->string) =
  object (self)
    val emp_val = x_init
    val mutable lc = None
    method is_avail : bool = match lc with
      | None -> false
      | Some _ -> true
    method is_empty : bool = lc == None
    method set (nl:'a) = lc <- Some nl
    method get :'a = match lc with
      | None -> emp_val
      | Some p -> p
    method reset = lc <- None
    method get_rm :'a = match lc with
      | None -> emp_val
      | Some p -> (lc <- None; p)
    method replace (nl:'a) =
      if not (self # is_empty) then self # set nl
      else ()
    method string_of : string = match lc with
      | None -> "Why None?"
      | Some l -> (epr l)
    method dump = print_endline ("\n store dump :"^(self#string_of))
  end;;

class ['a] store_debug (x_init:'a) (epr:'a->string) =
  object (self)
    inherit ['a] store x_init epr as super
    method reset = 
      if super # is_avail then
        begin
          print_endline ("reset:"^self#get);
          super # reset
        end
    method get_rm :'a = 
      print_endline ("get_rm:"^self#get);
      super # get_rm
  end;;

(* this will be set to true when we are in error explanation module *)
class failure_mode =
  object
    inherit [bool] store false string_of_bool
  end;;

class prog_loc =
  object
    inherit [loc] store no_pos string_of_loc
    method string_of_pos : string = match lc with
      | None -> "None"
      | Some l -> (string_of_pos l.start_pos)
  end;;

let method_name_of s =
  try 
    let hashtag_index = String.index s '#' in
    String.sub s 0 hashtag_index
  with _ -> s 

let is_equal call_name s =
  String.compare call_name (method_name_of s) = 0

class last_posn_cls =
  object (self)
    val last_posn = new store ("", "") (fun (x, _) -> "("^x^")")
    method reset (s: string): unit =
      if last_posn # is_avail then
        let last_call_site, last_call_name = last_posn # get in
        (* let () = print_endline ("last_posn reset: " ^ s ^ " @" ^ last_call_site ^ ":" ^ last_call_name) in *)
        if is_equal last_call_name s then last_posn # reset
        else ()
      else ()
    method get_rm (s: string) = 
      let last_call_site, last_call_name = last_posn # get in
      (* let () = print_endline ("last_posn get_rm: " ^ s ^ " @" ^ last_call_site ^ ":" ^ last_call_name) in *)
      if is_equal last_call_name s then
        let () = last_posn # reset in
        last_call_site 
      else ""
    method get (s: string) = 
      let last_call_site, last_call_name = last_posn # get in
      if is_equal last_call_name s then last_call_site 
      else ""
    method set_name (s: string) = 
      let last_call_site, last_call_name = last_posn # get in
      if last_call_name = "" then last_posn # replace (last_call_site, s)
    method set_posn pos = 
      last_posn # set (pos, "")
  end;;

let last_posn = new last_posn_cls
(* let last_posn = new store(* _debug *) ("", "") (fun (x, _) -> "("^x^")") *)

(*Some global vars for logging*)
let proving_loc  = new prog_loc
let post_pos = new prog_loc

let entail_pos = ref no_pos
let set_entail_pos p = entail_pos := p

(* what is this flag for? *)
let z_debug_flag = ref false

let buildA s i = s^"#"^(string_of_int i);;
let build_loc_str s i = "**"^(buildA s i)^":";;
let store_loc_str s i =
  if !z_debug_flag then
    let n = buildA s i 
    in last_posn # set_posn n;;
