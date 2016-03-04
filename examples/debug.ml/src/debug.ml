let debug_precise_trace = ref false
let debug_trace_log = ref false
let debug_trace_log_num = ref (-2)
let enable_counters = ref false
let profiling = ref false
let profile_threshold = 0.5

module Basic =
(* basic utilities that can be opened *)
struct

  exception Bad_string
  exception Bail

  let print_endline_quiet s = print_endline s

  let hash_to_list ht =
    Hashtbl.fold (fun a b c -> (a,b)::c) ht []

  let list_cnt_sort_dec l = 
    List.sort (fun (_,a) (_,b) -> 
        if a=b then 0
        else if a>b then -1
        else 1) l

  let sort_gen_list score vlist =
    let compare v1 v2 =
      let n1 = score v1 in
      let n2 = score v2 in
      if n1<n2 then -1
      else if n1=n2 then 0
      else 1 in
    List.sort compare vlist

  let string_of_pair (p1:'a->string) (p2:'b->string) ((a,b):'a * 'b) : string = 
    "("^(p1 a)^","^(p2 b)^")"

  let rec remove_dups n = 
    match n with
        [] -> []
      | q::qs -> if (List.mem q qs) then remove_dups qs else q::(remove_dups qs)

  let pr_id x = x

  let print_flush s = print_endline (s); flush stdout

  let pr_no x = "?"
  let pr_none x = "?"

  let pr_unit x = "()"

  let pr_option f x = match x with
    | None -> "None"
    | Some v -> "Some("^(f v)^")"

  let pr_opt = pr_option 

  let pr_opt_int = pr_option string_of_int

  let pr_pair f1 f2 (x,y) = "("^(f1 x)^","^(f2 y)^")"

  let pr_triple f1 f2 f3 (x,y,z) = "("^(f1 x)^","^(f2 y)^","^(f3 z)^")"

  let pr_quad f1 f2 f3 f4 (x,y,z,z2) = "("^(f1 x)^","^(f2 y)^","^(f3 z)^","^(f4 z2)^")"
  let pr_penta f1 f2 f3 f4 f5 (x,y,z,z2,z3) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^")"
  let pr_hexa f1 f2 f3 f4 f5 f6 (x,y,z,z2,z3,z4) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^",6:"^(f6 z4)^")"

  let pr_hepta f1 f2 f3 f4 f5 f6 f7 (x,y,z,z2,z3,z4,z5) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^",6:"^(f6 z4)^",7:"^(f7 z5)^")"

  let pr_quad_ln f1 f2 f3 f4 (x,y,z,z2) = "("^(f1 x)^"\n,2:"^(f2 y)^"\n,3:"^(f3 z)^"\n,4:"^(f4 z2)^")"
  let pr_penta_ln f1 f2 f3 f4 f5 (x,y,z,z2,z3) = "("^(f1 x)^"\n,2:"^(f2 y)^"\n,3:"^(f3 z)^"\n,4:"^(f4 z2)^"\n,5:"^(f5 z3)^")"
  let pr_hexa_ln f1 f2 f3 f4 f5 f6 (x,y,z,z2,z3,z4) = "("^(f1 x)^"\n,2:"^(f2 y)^"\n,3:"^(f3 z)^"\n,4:"^(f4 z2)^"\n,5:"^(f5 z3)^"\n,6:"^(f6 z4)^")"

  let pr_lst s f xs = String.concat s (List.map f xs)

  let pr_list_brk_sep open_b close_b sep f xs  = open_b ^(pr_lst sep f xs)^close_b

 let pr_list_brk open_b close_b f xs  = open_b ^(pr_lst "," f xs)^close_b
 let pr_list f xs = pr_list_brk "[" "]" f xs
 let pr_list_angle f xs = pr_list_brk "<" ">" f xs
 let pr_list_round f xs = pr_list_brk "(" ")" f xs
 let pr_list_ln f xs = "["^(pr_lst ",\n" f xs)^"]"

 let pr_list_mln f xs = (pr_lst "\n--------------\n" f xs)

 let map_opt f x = match x with 
   | None -> None
   | Some v -> Some (f v)

 let map_opt_res f x = match x with 
   | None -> (None,[])
   | Some v -> let r1,r2 = f v in (Some r1,r2)
   
 let fold_opt f x = match x with 
   | None -> []
   | Some v -> (f v)

 let map_l_snd f x = List.map (fun (l,c)-> (l,f c)) x
 let fold_l_snd f x = List.fold_left (fun a (_,c)-> a@(f c)) []  x
 let fold_l_snd_f fj f st x = List.fold_left (fun a (_,c)-> fj a (f c)) st  x
 let map_l_snd_res f x = List.split (List.map (fun (l,c) -> let r1,r2 = f c in ((l,r1),r2)) x)
 let exists_l_snd f x = List.exists (fun (_,c)-> f c) x
 let all_l_snd f x = List.for_all (fun (_,c)-> f c) x
 
 let add_str s f xs = s^":"^(f xs)

  let opt_to_list o = match o with
    | None -> []
    | Some a -> [a]

  let opt_list_to_list o = match o with
    | None -> []
    | Some a -> a

  let fnone (c:'a):'a option = None

  let is_empty l = match l with [] -> true | _ -> false

  let rec last_ne l a  = match l with
    | [] -> a
    | x::xs -> last_ne l x

  let last l = match l with
    | [] -> raise Not_found
    | x::xs -> last_ne l x

  let spacify i = 
    let s' z = List.fold_left (fun x y -> x ^ i ^ y) "" z in
    function [] -> ""
      | [x] -> x
      | x::xs -> x ^ (s' xs)

   (*
    first component of returned value contains the first i values from the list
    second component contains the rest
  *)
  let rec split_at (xs : 'a list) (i : int) : ('a list * 'a list) =
    if i = 0 then ([], xs)
    else
	  let a, b = split_at (List.tl xs) (i-1) in
	  ((List.hd xs) :: a, b) 

  let rec split3 (l : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list = match l with
    | (h1, h2, h3) :: rest ->
	      let l1, l2, l3 = split3 rest in
		  (h1::l1, h2::l2, h3::l3)
    | [] -> ([], [], [])

  let rec combine3 a b c = match (a, b, c) with
    | (ah::arest, bh::brest, ch::crest) ->
	      let l = combine3 arest brest crest in
		  (ah, bh, ch)::l
    | ([], [], []) -> []
    | _ -> failwith ("combine3: combining lists with different lengths")

  let rec map3 (f : 'a -> 'b -> 'c -> 'd) (a0 : 'a list) (bs : 'b list) (cs : 'c list) : 'd list = 
    match (a0, bs, cs) with
	  | (a :: r1, b :: r2, c :: r3) ->
		    let r = map3 f r1 r2 r3 in
		    (f a b c) :: r
	  | [], [], [] -> []
	  | _ -> failwith ("map3: mapping lists with different lengths.")

  let rec map4 (f : 'a -> 'b -> 'c -> 'd -> 'e) (a0 : 'a list) (bs : 'b list) (cs : 'c list) (ds : 'd list) : 'e list = 
    match (a0, bs, cs, ds) with
	  | (a :: r1, b :: r2, c :: r3, d:: r4) ->
		    let r = map4 f r1 r2 r3 r4 in
		    (f a b c d) :: r
	  | [], [], [], [] -> []
	  | _ -> failwith ("map4: mapping lists with different lengths.")


  let rec repeat (v : 'a) (n : int) : 'a list =
    if n <= 0 then []
    else v :: (repeat v (n-1))

end;;

module BList =
struct

  (* List-handling stuff *)

  let string_of_f (f:'a->string) (ls:'a list) : string = 
    ("["^(String.concat "," (List.map f ls))^"]")
    
  (** Split the list of length k>=1 into a pair consisting of
      the list of first k-1 elements and the last element. *)
  let rec firsts_last xs = match xs with
    | [] -> failwith "Gen.first_lasts: empty list"
    | [x] -> ([],x)
    | x::xs1 ->
          let (fs,l) = firsts_last xs1 in
          (x::fs,l)

  let rec take n l  = if n<=0 then [] else 
    match l with
      | h::t -> h::(take (n-1) t)
      | [] -> []
            
  let rec drop n l  = if n<=0 then l else
    match l with
      | h::t -> (drop (n-1) t)
      | [] -> []

  (* let remove_elem e l = List.filter (fun x -> x != e) l *)

  (* let rec remove_dups n =  *)
  (*   match n with *)
  (*       [] -> [] *)
  (*     | q::qs -> if (List.mem q qs) then remove_dups qs else q::(remove_dups qs) *)

  (* let mem f x l = List.exists (f x) l *)

  (* let rec find_dups n =  *)
  (*   match n with *)
  (*     | [] -> [] *)
  (*     | q::qs -> if (List.mem q qs) then q::(find_dups qs) else find_dups qs *)

  (* let subset l1 l2 =  *)
  (*   List.for_all (fun x -> (List.mem x l2)) l1 *)

  (* let disjoint l1 l2 =  *)
  (*   List.for_all (fun x -> not (List.mem x l2)) l1 *)

  (* let overlap eq l1 l2 =  *)
  (*   List.exists (fun x -> (List.mem x l2)) l1 *)

  (* let intersect l1 l2 = *)
  (*   List.filter (fun x -> List.mem x l2) l1 *)


  (* let difference l1 l2 = *)
  (*   List.filter (fun x -> not (List.mem x l2)) l1 *)


  (* let list_equal l1 l2 =  *)
  (*   let l = (List.length (intersect l1 l2)) in *)
  (*   ((List.length l1) =  l) && (l = (List.length l2)) *)


  let find_index (f : 'a -> bool) (xs0 : 'a list) : (int * 'a) = 
    let rec helper xs n = match xs with
	  | e :: rest -> 
		    if f e then (n, e)
		    else helper rest (n + 1)
	  | _ -> raise Not_found
    in
    helper xs0 0

  let rec list_last l = match l with
    | h::[] -> h
    | _::t -> (list_last t)
    | [] -> failwith "Gen.list_last: empty list"

  let remove_elem_eq eq e l = List.filter (fun x -> not(eq x e)) l 

  let mem_eq eq x l = List.exists (eq x) l

  let rec remove_dups_eq eq n = 
    match n with
        [] -> []
      | q::qs -> if (mem_eq eq q qs) then remove_dups_eq eq qs else q::(remove_dups_eq eq qs)

  let rec check_dups_eq eq n = 
    match n with
      | [] -> false
      | q::qs -> if (List.exists (fun c-> eq q c) qs) then true  else check_dups_eq eq qs 

  let check_no_dups_eq eq n = not(check_dups_eq eq n)

  let subset_eq eq l1 l2 =
    List.for_all (fun x -> (mem_eq eq x l2)) l1

  let disjoint_eq eq l1 l2 =
    List.for_all (fun x -> not (mem_eq eq x l2)) l1

  let overlap_eq eq l1 l2 =
	if (l2 == []) then false
	else List.exists (fun x -> (mem_eq eq x l2)) l1

  let rec find_dups_eq eq n = 
    match n with
      | [] -> []
      | q::qs -> if (List.exists (eq q) qs) then q::(find_dups_eq eq qs) else find_dups_eq eq qs

  let rec find_one_dup_eq eq (xs : 'a list) =
    match xs with
	  | [] -> []
	  | x::rest -> if List.exists (eq x) rest then [x] else find_one_dup_eq eq rest

  let mem_eq eq x ls =
    List.exists (fun e -> eq x e) ls

  let intersect_eq eq l1 l2 =
    List.filter (fun x -> List.exists (eq x) l2) l1  

  let difference_eq eq l1 l2 =
    List.filter (fun x -> not (List.exists (eq x) l2)) l1

  let diff_split_eq eq l1 l2 = 
    List.partition (fun x -> not (List.exists (eq x) l2)) l1
    
  let list_subset_eq eq l1 l2 = 
    let l = (List.length (difference_eq eq l1 l2)) in
    l==0

  (* change name to setequal *)
  let list_setequal_eq eq l1 l2 = 
    (list_subset_eq eq l1 l2) && (list_subset_eq eq l2 l1) 

  let list_equiv_eq eq l1 l2 = 
    try
      List.for_all2 eq l1 l2
    with _ -> false

  let rec list_find (f:'a -> 'b option) l = match l with 
    | [] -> None
    | x::xs -> match f x with
        | None -> list_find f xs
        | Some s -> Some s

end;;

include Basic

exception Stack_Error

class ['a] stack  =
   object 
     val mutable stk = []
     method push (i:'a) = 
       begin
         stk <- i::stk
       end
     method get_stk  = stk (* return entire content of stack *)
     method override_stk newstk  = stk <- newstk 
       (* override with a new stack *)
     method pop = match stk with 
       | [] -> print_string "ERROR : popping empty stack"; 
               raise Stack_Error
       | x::xs -> stk <- xs
     method pop_top = match stk with 
       | [] -> print_string "ERROR : popping empty stack"; 
               raise Stack_Error
       | x::xs -> stk <- xs; x
     method top : 'a = match stk with 
       | [] -> print_string "ERROR : top of empty stack"; 
               raise Stack_Error
       | x::xs -> x
     method pop_no_exc = match stk with 
       | [] -> () 
       | x::xs -> stk <- xs
     method is_empty = stk == []
     method len = List.length stk
     (* method len_recent = recent *)
     method reverse = stk <- List.rev stk
     method reverse_of = List.rev stk
     method mem (i:'a) = List.mem i stk 
     method mem_eq eq (i:'a) = List.exists (fun b -> eq i b) stk 
     (* method exists (i:'a) = List.mem i stk  *)
     (* method exists_eq eq (i:'a) = List.exists (fun b -> eq i b) stk  *)
     method exists f = List.exists f stk 
     method push_list (ls:'a list) =  stk <- ls@stk
     method pop_list (ls:'a list) = 
       stk <- BList.drop (List.length ls) stk
     method reset = stk <- []
   end;;

class ['a] stack_pr (epr:'a->string) (eq:'a->'a->bool)  =
   object 
     inherit ['a] stack as super
     val elem_pr = epr 
     val elem_eq = eq 
     method string_of = Basic.pr_list_ln elem_pr stk
     method string_of_no_ln = Basic.pr_list elem_pr stk
     method string_of_reverse = let _ = super#reverse  in
                                Basic.pr_list_ln elem_pr stk
     method string_of_reverse_log = let _ = super#reverse  in
                                Basic.pr_list_mln elem_pr stk
    method dump_no_ln =
      begin
        let s = super#reverse_of  in
        List.iter (fun e -> print_string (elem_pr e)) s
      end
     method mem (i:'a) = List.exists (elem_eq i) stk
     method overlap (ls:'a list) = 
	   if (ls == []) then false
	   else List.exists (fun x -> List.exists (elem_eq x) ls) stk
   end;;


class ['a] stack_filter (epr:'a->string) (eq:'a->'a->bool) (fil:'a->bool)  =
   object 
     inherit ['a] stack_pr epr eq as super
     val filter_fn = fil
     method filter = stk <- List.filter fil stk
     method string_of_reverse_log_filter = 
       stk <- List.filter fil stk;
       super#string_of_reverse_log
   end;;

class ['a] stack_noexc (x_init:'a) (epr:'a->string) (eq:'a->'a->bool)  =
   object 
     inherit ['a] stack_pr epr eq
     val emp_val = x_init
     method top_no_exc : 'a = match stk with 
       | [] ->  emp_val
       | x::xs -> x
   end;;

class counter x_init =
   object 
     val mutable ctr = x_init
     method get : int = ctr
     method inc = ctr <- ctr + 1
     method inc_and_get = ctr <- ctr + 1; ctr
     method add (i:int) = ctr <- ctr + i
     method reset = ctr <- 0
     method string_of : string= (string_of_int ctr)
   end;;

class ctr_call x_init =
  object 
    inherit counter x_init as super
    val mutable last_call = ""
    method next_call =
      let c = super # inc_and_get in
      let () = last_call <- "@"^(string_of_int c) in
      c
    method get_last_call =
      last_call
  end

module StackTrace =
struct 
  (* keep track of calls being traced by ho_debug *)
  let ctr = new ctr_call 0
    
  (* type stack = int list *)
  (* stack of calls being traced by ho_debug *)
  let debug_stk = new stack_noexc (-2) string_of_int (=)

  let dd_stk = new stack

  (* let force_dd_print () = *)
  (*   let d = dd_stk # get_stk in *)
  (*   debug_stk # overlap d *)

  let is_same_dd_get () =
    if dd_stk # is_empty then None
    else 
      let v1 = dd_stk # top in
      let v2 = debug_stk # top in
       if (v1==v2) then Some v1 else None

  (* pop last element from call stack of ho debug *)
  let pop_call () = 
    let () = match is_same_dd_get () with
      | None -> ()
      | Some no -> 
        let n2 = !debug_trace_log_num in
        if n2<0 || n2=no then dd_stk # pop
        else ()
    in
    let () = debug_stk # pop in
    (* let () = print_string "after pop_call" in *)
    (* let () = force_dd_print() in *)
    ()

  let trace_exception s exc =
    if !VarGen.trace_exc then Basic.print_endline_quiet ("Exception("^s^"):"^(Printexc.to_string exc))

    (* call f and pop its trace in call stack of ho debug *)
  let pop_aft_apply_with_exc s (f:'a->'b) (e:'a) : 'b =
    let r = try 
      (f e)
    with exc -> 
        begin 
          trace_exception s exc;
          pop_call(); 
          raise exc
        end
    in pop_call(); r


  (* call f and pop its trace in call stack of ho debug *)
  let pop_aft_apply_with_exc_no s (f:'a->'b) (e:'a) : 'b =
    try 
      let r = (f e) in
      if !debug_precise_trace then debug_stk # pop; 
      r
    with exc -> 
        begin
          trace_exception s exc;
          if !debug_precise_trace then debug_stk # pop; 
          raise exc
        end

  (* string representation of call stack of ho_debug *)
  let string_of () : string =
    let h = debug_stk#get_stk in
    (* ("Length is:"^(string_of_int (List.length h))) *)
    String.concat "@" (List.map string_of_int (List.filter (fun n -> n>0) h) )

  let push_no_call () =
    if !debug_precise_trace then debug_stk # push (-3)
    else ()

  (* returns @n and @n1;n2;.. for a new call being debugged *)
  let push_call_gen (os:string) (flag_detail:bool) : (string * string) = 
    (* let () = ctr#inc in *)
    let v = ctr#next_call in
    let () = debug_stk#push v in
    if flag_detail || !debug_trace_log then 
      if !debug_trace_log_num<0 || !debug_trace_log_num=v then
        dd_stk#push v;
    let lc = ctr # get_last_call in
    let s = os^lc in
    let h = os^"@"^string_of() in
    (* let () = print_endline ("\npush_call_gen:"^os^(string_of_bool flag_detail)^s) in *)
    (* let () = force_dd_print() in *)
    s,h

  (* push call without detailed tracing *)
  let push_call (os:string) : (string * string) = 
    push_call_gen os false

  (* push call with detailed tracing *)
  let push_call_dd (os:string) : (string * string) = 
    push_call_gen os true

  let is_same_dd () =
    match (is_same_dd_get()) 
    with | None -> false
      | _ -> true


end;;

(* open Globals *)
type loc =  {
    start_pos : Lexing.position (* might be expanded to contain more information *);
    mid_pos : Lexing.position;
    end_pos : Lexing.position;
  }

let no_pos = 
	let no_pos1 = { Lexing.pos_fname = "";
				   Lexing.pos_lnum = 0;
				   Lexing.pos_bol = 0; 
				   Lexing.pos_cnum = 0 } in
	{start_pos = no_pos1; mid_pos = no_pos1; end_pos = no_pos1;}

open VarGen

let debug_on = ref false
let devel_debug_on = ref false
let devel_debug_steps = ref false
let devel_debug_sleek_proof = ref (-1)
let debug_print = ref false (* to support more printing for debugging *)
let devel_debug_print_orig_conseq = ref false
let debug_pattern_on = ref false
let debug_pattern = ref (Str.regexp ".*")
let trace_on = ref true
let call_threshold = ref 10
let dump_calls = ref false
let dump_callers_flag = ref false
let dump_calls_all = ref false
let call_str = ref ""

let z_debug_arg = ref (None:Str.regexp option)

let () = if !compete_mode then
    begin
      trace_on := false;
    end
let log_devel_debug = ref false
let debug_log = Buffer.create 5096

let clear_debug_log () = Buffer.clear debug_log
let get_debug_log () = Buffer.contents debug_log

(* debugging facility for user *)

(* used to enable the printing of the original consequent while devel debugging. By default, orig_conseq is disabled*)
let enable_dd_and_orig_conseq_printing () =
  devel_debug_on := true;
  devel_debug_print_orig_conseq :=  true

let string_of_pos (pos:loc) =
  pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^ ": "^(string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol))^": "

let print s = if !debug_on then (print_string ("\n\n!!!" ^ s); flush stdout) else ()

let pprint msg (pos:loc) = 
  let tmp = pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^ ": "^ (string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol))^ ": " ^ msg in
  print tmp

(* system development debugging *)
let ho_print flag (pr:'a->string) (m:'a) : unit =
  let d = StackTrace.is_same_dd_get () in
  (* let () = print_endline ("\ndd_get:"^((pr_option string_of_int) d)) in *)
  (* WN : should we use && or || *)
  if !compete_mode then ()
  else if (flag (* !devel_debug_on *)  ||  not(d==None)) then 
    let s = (pr m) in
    let msg = match d with 
      | None -> ("\n!!!" ^ s)
      | Some cid -> ("\n@"^(string_of_int cid)^"!"^ s) 
    in
    (* if !log_devel_debug then  *)
    (*   Buffer.add_string debug_log msg *)
    (* else *)
    (print_string msg; flush stdout)
  else ()

(* system development debugging *)
let devel_print s = 
  ho_print !devel_debug_on (fun x -> x) s 
(* let d = StackTrace.is_same_dd_get () in *)
(*   if !devel_debug_on  || not(d==None) then  *)
(*     let msg = match d with  *)
(*       | None -> ("\n!!!" ^ s) *)
(*       | Some cid -> ("\n@"^(string_of_int cid)^"!"^ s)  *)
(*     in *)
(*     if !log_devel_debug then  *)
(*       Buffer.add_string debug_log msg *)
(*     else *)
(*       (print_string msg; flush stdout) *)
(*   else () *)

let prior_msg pos =
  let tmp = pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^ ": " ^ (string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol)) ^ ": " in
  let tmp = if is_no_pos !VarGen.entail_pos then tmp 
    else (tmp^"[entail:"^(string_of_int !VarGen.entail_pos.start_pos.Lexing.pos_lnum)^"]"^"[post:"^(string_of_int (VarGen.post_pos#get).start_pos.Lexing.pos_lnum)^"]") 
  in tmp

let devel_pprint (msg:string) (pos:loc) =
  let flag = !devel_debug_on in
  ho_print flag (fun m -> (prior_msg pos)^m) msg

let devel_hprint (pr:'a->string) (m:'a) (pos:loc) = 
  let flag = !devel_debug_on in
  ho_print flag (fun x -> (prior_msg pos)^(pr x)) m

let devel_zprint msg (pos:loc) =
  let flag = !devel_debug_on in
  ho_print flag (fun m -> (prior_msg pos)^(Lazy.force m)) msg

let catch_exc m f x = 
  try 
    f x
  with e -> (print_endline_quiet m; flush stdout; raise e)

let dinfo_zprint m p = devel_zprint m p
let dinfo_hprint pr m p  = devel_hprint pr m p
let dinfo_pprint m p = devel_pprint m p

let binfo_pprint (msg:string) (pos:loc) =
  let s = if !devel_debug_on then (prior_msg pos) else " " in
  let flag = !trace_on || !devel_debug_on in
  ho_print flag (fun m -> s^m) msg

let winfo_pprint (msg:string) (pos:loc) =
  let s = if !devel_debug_on then (prior_msg pos) else " " in
  let flag = !trace_on || !devel_debug_on in
  ho_print flag (fun m -> s^m) ("**WARNING**"^msg)

let binfo_hprint (pr:'a->string) (m:'a) (pos:loc) = 
  let s = if !devel_debug_on then (prior_msg pos) else " " in
  let flag = !trace_on || !devel_debug_on in
  ho_print flag (fun x -> s^(pr x)) m

let binfo_zprint msg (pos:loc) =
  let s = if !devel_debug_on then (prior_msg pos) else " " in
  let flag = !trace_on || !devel_debug_on in
  ho_print flag (fun m -> s^(Lazy.force m)) msg


let binfo_start (msg:string) =
  binfo_pprint "**********************************" no_pos;
  binfo_pprint ("**** "^msg^" ****") no_pos;
  binfo_pprint "**********************************" no_pos

let binfo_end (msg:string) =
  binfo_pprint "**********************************" no_pos;
  binfo_pprint ("**** end of "^msg^" ****") no_pos;
  binfo_pprint "**********************************" no_pos

let dinfo_start (msg:string) =
  dinfo_pprint "**********************************" no_pos;
  dinfo_pprint ("**** "^msg^" detected ****") no_pos;
  dinfo_pprint "**********************************" no_pos

let dinfo_end (msg:string) =
  dinfo_pprint "**********************************" no_pos;
  dinfo_pprint ("**** end of "^msg^" ****") no_pos;
  dinfo_pprint "**********************************" no_pos

let ninfo_zprint m p = ()
let ninfo_hprint pr m p  = ()
let ninfo_pprint m p = ()

(*
  -- -v:10-50 (for details + all tracing + omega)
  -- -v:1-9 (for details only)
  -- -v:50.. (for tracing only)
  -- -v:-1 (minimal tracing)
  -- -v:-2..(exact tracing)
*)

let gen_vv_flags d =
  let m = !VarGen.verbose_num in
  let (flag,str) =
    if d<0 then (m==d,"EXACT:")
    else if m>50 then (d>=m,"DEBUG:")
    else if m<10 then (m>=d,"")
    else if d>=50 then (true,"DEBUG_"^(string_of_int d)^":")
    else (m>=d,"") in
  (flag,str)

let verbose_hprint (d:int) (p:'a -> string) (arg:'a)  =
  let (flag,str)=gen_vv_flags d in
  ho_print flag (add_str str p) arg

(* let verbose_pprint (d:int) (msg:string)  = *)
(*   verbose_hprint d (fun m -> m) msg *)

(* let verbose_pprint (d:int) (msg)  = *)
(*   verbose_hprint d (fun m -> m) msg *)

let vv_pprint d msg = verbose_hprint d (fun m -> m) msg

let vv_hprint d f arg = verbose_hprint d f arg

let vv_zprint d lmsg = 
  verbose_hprint d (fun x -> Lazy.force x) lmsg

let vv_plist d ls = 
  let (flag,str) = gen_vv_flags d in
  let rec helper ls =
    match ls with
    | [] -> ()
    | ((m,y)::xs) ->
      begin
        (ho_print flag (fun msg -> str^m^":"^msg) y)
      ; helper xs
      end
  in helper ls

let vv_hdebug f arg = vv_hprint 200 f arg 

(* less tracing *)
let vv_pdebug msg = vv_hdebug (fun m -> m) msg

let vv_debug msg = vv_pdebug msg

(* detailed tracing *)
let vv_trace msg = vv_hprint 100 (fun m -> m) msg

let vv_zdebug msg = vv_hdebug (fun x -> Lazy.force x) msg

let vv_result (s:string) (d:int) ls =
  vv_pprint d (">>>>>>>>>"^s^">>>>>>>>>");
  vv_plist d ls;
  vv_pprint d (">>>>>>>>>"^s^">>>>>>>>>")

let trace_pprint (msg:string) (pos:loc) : unit = 
  ho_print !devel_debug_on (fun a -> " "^a) msg

let trace_hprint (pr:'a->string) (m:'a) (pos:loc) = 
  ho_print !devel_debug_on (fun x -> " "^(pr x)) m

let trace_zprint m (pos:loc) = 
  ho_print !devel_debug_on (fun x -> Lazy.force x) m

let tinfo_zprint m p = trace_zprint m p
let tinfo_hprint pr m p  = trace_hprint pr m p
let tinfo_pprint m p = trace_pprint m p

let info_pprint (msg:string) (pos:loc) : unit =
  let flag = not(!compete_mode) in
  ho_print flag (fun a -> " "^a) msg

let info_hprint (pr:'a->string) (m:'a) (pos:loc) = 
  let flag = not(!compete_mode) in
  ho_print flag (fun x -> " "^(pr x)) m

let info_ihprint (pr:'a->string) (m:'a) (pos:loc) =
  let flag = not(!compete_mode) in
  if !VarGen.sap then ho_print flag (fun x -> " "^(pr x)) m
  else ()

let info_zprint m (pos:loc) = 
  let flag = not(!compete_mode) in
  ho_print flag (fun x -> Lazy.force x) m

(* let devel_zprint msg (pos:loc) = *)
(* 	lazy_print (prior_msg pos) msg *)

(* let trace_zprint msg (pos:loc) =  *)
(* 	lazy_print (fun () -> " ") msg *)

let y_binfo_pprint msg = binfo_pprint msg no_pos
let y_binfo_zprint msg = binfo_zprint msg no_pos
let y_binfo_hprint pr msg = binfo_hprint pr msg no_pos

let y_dinfo_pprint msg = dinfo_pprint msg no_pos
let y_dinfo_zprint msg = dinfo_zprint msg no_pos
let y_dinfo_hprint pr msg = dinfo_hprint pr msg no_pos

let y_tinfo_pprint msg = tinfo_pprint msg no_pos
let y_tinfo_zprint msg = tinfo_zprint msg no_pos
let y_tinfo_hprint pr msg = tinfo_hprint pr msg no_pos

let y_winfo_pprint msg = winfo_pprint msg no_pos

let print_info prefix str (pos:loc) = 
  let tmp = "\n" ^ prefix ^ ":" ^ pos.start_pos.Lexing.pos_fname ^ ":" ^ (string_of_int pos.start_pos.Lexing.pos_lnum) ^": " ^ (string_of_int (pos.start_pos.Lexing.pos_cnum-pos.start_pos.Lexing.pos_bol)) ^": " ^ str ^ "\n" in
  print_string tmp; flush stdout


open StackTrace

(* let ho_2_opt_aux (loop_d:bool) (test:'z -> bool) (s:string) (pr1:'a->string) (pr2:'b->string) (pr_o:'z->string)  (f:'a -> 'b -> 'z)  *)
(*       (e1:'a) (e2:'b) : 'z = *)
(*   let s,h = push s in *)
(*   (if loop_d then print_string (h^" inp :"^(pr1 e1)^"\n")); *)
(*   let r = try *)
(*     pop_ho (f e1) e2  *)
(*   with ex ->  *)
(*       let () = print_string (h^"\n") in *)
(*       let () = print_string (s^" inp1 :"^(pr1 e1)^"\n") in *)
(*       let () = print_string (s^" inp2 :"^(pr2 e2)^"\n") in *)
(*       let () = print_string (s^" Exception"^(Printexc.to_string ex)^"Occurred!\n") in *)
(*       raise ex in *)
(*   if not(test r) then r else *)
(*     let () = print_string (h^"\n") in *)
(*     let () = print_string (s^" inp1 :"^(pr1 e1)^"\n") in *)
(*     let () = print_string (s^" inp2 :"^(pr2 e2)^"\n") in *)
(*     let () = print_string (s^" out :"^(pr_o r)^"\n") in *)
(*     r *)

(* ss with at least one argument *)
let pick_front n ss =
  let rec aux n ss  =
    match ss with
    | [] -> ss
    | s::ss -> let m=String.length s in
      if n>=m then s::aux (n-m) ss 
      else []
  in match ss with 
  | [] -> ss
  | s::ss -> s::(aux (n-(String.length s)) ss)

(* module type LABEL_TYPE = *)
(*     sig *)
(*       type a *)
(*       type t  *)
(*       val unlabelled : t  *)
(*       val is_unlabelled : t -> bool (\* is this unlabelled *\) *)
(*       val norm : t -> t (\* sort a label *\) *)
(*       val is_compatible : t -> t -> bool *)
(*       val is_compatible_rec : t -> t -> bool *)
(*       (\* val comb_identical : t -> t -> t (\\* combine two identical labels *\\) *\) *)
(*       val comb_norm : int -> t -> t -> t (\* combine two normalised labels *\) *)
(*       val string_of : t -> string *)
(*       val compare : t -> t -> int *)
(*       val singleton : a -> t *)
(*       val convert : string -> lst_pair -> t *)
(*     end;; *)



module DebugCore  =
struct
  type debug_option =
    | DO_None
    | DO_Trace
    | DO_Loop
    | DO_Both
    | DO_Normal

  let debug_map = Hashtbl.create 50

  let regexp_line = ref []
  let regexp_line_str = ref []

  let z_debug_file = ref ""
  (* let z_debug_regexp = ref None *)
  let mk_debug_arg s =
    let re = Str.regexp s in
    z_debug_arg := Some re

  (*let debug_file = open_in_gen [Open_creat] 0o666 ("z-debug.log")*)
  let debug_file ()=
    let get_path s = 
      if String.contains s '/' then
        let i = String.rindex s '/' in
        String.sub s 0 (i+1)
      else ""
    in
    let fname = !z_debug_file in
    (* print_endline ("Debug File : "^fname); *)
    let n = String.length fname in
    if n>1 && fname.[0]='$' then
      begin
        (* z_debug_regexp := Some fname; *)
        let re = String.sub fname 1 (n-1) in
        regexp_line_str := [re];
        None
      end
    else
      begin
        let debug_conf = "./" ^ fname in
        (* let () = print_endline (debug_conf) in *)
        let global_debug_conf =
          if (Sys.file_exists debug_conf) then
            debug_conf
          else (get_path Sys.executable_name) ^ (String.sub debug_conf 2 ((String.length debug_conf) -2))
        in
        (* let () = print_endline global_debug_conf in *)
        try
          Some(open_in (global_debug_conf))
        with _ ->
          begin
            print_endline_quiet ("WARNING : cannot find debug file "^fname); 
            z_debug_flag:=false;
            None
          end
      end

  let read_from_debug_file chn : string list =
    let line = ref [] in
    (* let quitloop = ref false in *)
    (try
       while true do
         let xs = (input_line chn) in
         let xs = String.trim xs in
         let n = String.length xs in
         (* let s = String.sub xs 0 1 in *)
         if n > 0 && xs.[0]!='#' (* String.compare s "#" !=0 *) then begin
           line := xs::!line;
         end;
         if n > 1 && xs.[0]=='$' (* String.compare s "#" !=0 *) then begin
           let xs = String.sub xs 1 (n-1) in
           (* regexp_line := (Str.regexp_case_fold xs)::!regexp_line; *)
           regexp_line_str := xs::!regexp_line_str;
         end;
       done;
     with _ -> ());
    !line

  (* let read_from_debug_file chn : string list = *)
  (*  ho_1 "read_from_debug_file" (fun _ -> "?") (pr_list (fun x -> x))read_from_debug_file chn *)

  let proc_option str =
    let rec aux str tr_flag lp_flag =
      match str with
      | [] -> (tr_flag,lp_flag)
      | s::str ->
        begin
          if String.compare s "Trace" == 0 then aux str true lp_flag 
          else if String.compare s "Loop" == 0 then aux str tr_flag true 
          else 
            let () = (print_endline ("Warning - wrong debug command :"^s)) 
            in aux str tr_flag lp_flag
        end
    in aux str false false

  let rec get_words str =
    let len = String.length str in
    try
      let l = String.index str ',' in
      let m = String.sub str 0 l in
      let rest = String.sub str (l+1) ((len) -l -1) in
      m::(get_words rest)
    with _ -> if len<4 then [] else [str] 

  (* let get_words str = *)
  (*   let pr_id x = x in *)
  (*   ho_1 "get_words" pr_id (pr_list pr_id) get_words str *)

  let add_entry_with_options entry_fn xs =
    List.iter (fun x ->
        try
          let l = String.index x ',' in
          let m = String.sub x 0 l in
          let split = String.sub x (l+1) ((String.length x) -l -1) in
          let opts = get_words split in
          (* let (tr_flag,lp_flag) = proc_option opts in *)
          let kind = match proc_option opts with
            | false,false -> DO_Normal
            | false,true -> DO_Loop
            | true,false -> DO_Trace
            | true,true -> DO_Both
          in
          let () = print_endline_quiet (m) in
          let () = print_endline_quiet (split) in
          (* let kind = if String.compare split "Trace" == 0 then DO_Trace else *)
          (*   if String.compare split "Loop" == 0 then DO_Loop else *)
          (*     DO_Normal *)
          entry_fn m kind
        with _ ->
          entry_fn x DO_Normal
      ) xs

  (* (\* let () = print_endline ((pr_list (fun x -> x)) xs) in *\) *)
  (* List.iter (fun x -> *)
  (*     try *)
  (*       let l = String.index x ',' in *)
  (*       let m = String.sub x 0 l in *)
  (*       let split = String.sub x (l+1) ((String.length x) -l -1) in *)
  (*       let opts = get_words split in *)
  (*       (\* let (tr_flag,lp_flag) = proc_option opts in *\) *)
  (*       let kind = match proc_option opts with *)
  (*         | false,false -> DO_Normal *)
  (*         | false,true -> DO_Loop *)
  (*         | true,false -> DO_Trace *)
  (*         | true,true -> DO_Both *)
  (*       (\* let () = print_endline (m) in *\) *)
  (*       (\* let () = print_endline (split) in *\) *)
  (*       (\* let kind = if String.compare split "Trace" == 0 then DO_Trace else *\) *)
  (*       (\*   if String.compare split "Loop" == 0 then DO_Loop else *\) *)
  (*       (\*     DO_Normal *\) *)
  (*       in *)
  (*       Hashtbl.add debug_map m kind *)
  (*     with _ -> *)
  (*     Hashtbl.add debug_map x DO_Normal *)
  (* ) xs *)

  let in_debug x =
    let x = String.trim x in
    let opt_k = 
      try
        Some(List.find (fun (re,k) -> Str.string_match re x 0) !regexp_line)
      with _ -> None in
    match opt_k with
    | Some (_,k) -> k
    | None ->
      begin
        try
          Hashtbl.find debug_map x
        with _ -> DO_None
      end


  (* let threshold = 20 in (\* print calls above this threshold *\) *)

  let debug_calls  =
    let len = 100 in
    let prefix = "%%%" in
    let pr_cnt (s, cnt) = s ^ (if cnt > 1 then " (" ^ (string_of_int cnt) ^ ")" else "") in
    let summarized_stack stk =
      let new_stk = new stack_pr (* "debug-calls" *) pr_cnt (==) in
      match (List.rev stk#get_stk) with
      | [] -> new_stk
      | hd::tl ->
        let ctr = ref 1 in
        let now = ref hd in
        List.iter (fun y ->
            if y = !now then incr ctr
            else 
              let () = new_stk#push (!now, !ctr) in
              let () = ctr := 1 in
              now := y) tl;
        new_stk
    in
    object (self)
      val len_act = len -1
      val arr =  Array.make (len+1) prefix
      val hcalls = Hashtbl.create 20
      val rec_calls = Hashtbl.create 10
      val calls =  Array.make (len+1) ""
      (* debug calls in the run-time state *)
      val overflow = prefix^"****************************************"
      val mutable lastline = "\n"
      val mutable rgx = None
      val stk_trace = new stack_pr (* "debug-calls(2)" *) pr_id (==)
      val stk_calls = new stack_pr (* "stk_calls" *) pr_id (==)
      val stk_callers = new stack
      val mutable offset = -1
      val mutable last_matched_len = max_int
      method dump_callers =
        let lst = stk_callers # get_stk in
        let lst = List.rev lst in
        print_endline "\nCALLERS TRACING";
        print_endline   "================";
        List.iter (fun xs ->
            print_endline_quiet ((pr_list pr_id) xs)
          ) lst
      method dump =
        let cnt = hash_to_list hcalls in
        let rcnt = hash_to_list rec_calls in
        let cnt = List.filter (fun (_,a) -> a>=(!call_threshold)) cnt in
        let cnt = list_cnt_sort_dec cnt in
        let rcnt = list_cnt_sort_dec rcnt in
        let pr = pr_list_brk_sep "" "" "\n" (pr_pair pr_id string_of_int) in
        if !dump_calls_all then
          begin
            print_endline "\nCALL TRACE";
            print_endline   "==========";
            stk_trace # push (lastline^"\n");
            (summarized_stack stk_trace) # dump_no_ln
          end;
        print_endline "\nDEBUGGED CALLS";
        print_endline   "==============";
        print_endline (string_of_int (List.length cnt));
        print_endline (pr cnt);
        if rcnt!=[] then
          begin
            print_endline "\nDEBUGGED SELF-REC CALLS";
            print_endline   "=======================";
            print_endline (pr rcnt)
          end;
        (* self # dump_callers *)
      method init =
        if (not !debug_pattern_on) then
          for i = 1 to len_act do
            arr.(i) <- arr.(i-1)^" "
          done;
        let cs = !call_str in
        if not(cs="") 
        then 
          begin
            (* print_endline ("rgx:"^(cs)); *)
            rgx <- Some (Str.regexp cs)
          end
        else 
          begin
            (* print_endline ("rgx(N):"^(cs)); *)
            ()
          end
      method str_match s =
        match rgx with
        | None -> true
        | Some rgx -> Str.string_match rgx s 0
      method add_to_hash ht s =
        try 
          let c = Hashtbl.find ht s in
          Hashtbl.replace ht s (c+1)
        with _ -> Hashtbl.add ht s 1
      method get dd_n s =
        (* pre : n>=0 *)
        let n = match rgx with
          | None -> dd_n
          | Some rgx -> 
            if offset>=0 && dd_n>offset then dd_n-offset
            else if Str.string_match rgx s 0 then (offset<-dd_n; 0)
            else (offset<-(-1); failwith "skipped") in
        if (n>len_act) then overflow
        else 
          begin
            calls.(n)<-s;
            if n>0 && s=calls.(n-1) then 
              begin
                self # add_to_hash rec_calls s;
                (* print_endline ("REC "^s) *)
              end;
            self # add_to_hash hcalls s;
            arr.(n)
          end
      method pop_call =
        begin
          if !dump_callers_flag (* z_debug_flag *) then
            (if stk_calls # is_empty then print_endline_quiet "WARNING stk_calls is empty";
            stk_calls # pop_no_exc)
        end
      method push_call s (call_site:string) =
          stk_calls # push s;
      method print_call s call_site =
        let spaces n = String.init n (fun _ -> ' ') in
        let is_match s = Str.string_match !debug_pattern s 0 in
        let matched_call s =
          let is_callee = (debug_stk # len) > last_matched_len in
          let is_match_s = is_match s in
          if is_match_s && !debug_pattern_on then
            (lastline <- (lastline ^ "\n...");
             last_matched_len <- (debug_stk#len));
          is_match_s || is_callee
        in
        begin
          (* self # push_call s call_site; *)
          try
            if (!debug_pattern_on && not (matched_call s))
            then last_matched_len <- max_int
            else (
              let deb_len = debug_stk # len in
              let len = if !debug_pattern_on then
                  (self#get (deb_len) s) ^ (spaces (deb_len - last_matched_len))
                else self # get (deb_len) s
              in
              if !dump_calls_all then
                begin
                  stk_trace # push lastline;
                  lastline <- "\n"^len^s
                end)
          with _ -> ()
        end
      method add_id id =
        begin
          let id_w_dot = id^"." in
          let get_callers_from_stk stk = 
            if !dump_callers_flag then
              begin
                if stk # is_empty then 
                  (print_endline_quiet "WARNING: call stk empty")
                else
                  let s = (stk # pop_top)^id in
                  let lst = stk # get_stk in
                  let () = stk # push s in
                  let nlst = (s^".")::lst in
                  let () = stk_callers # push nlst in
                  ()
              end
          in
          let () = get_callers_from_stk stk_calls in
          if !dump_calls_all then
            begin
              lastline <- lastline^id_w_dot;
            end
        end
    end

  let dump_debug_calls () = 
    begin
      if !dump_calls then debug_calls # dump;
      if !dump_callers_flag then debug_calls # dump_callers
    end

  let read_main () =
    let () = debug_calls # init in
    let xs = match debug_file() with
      | Some c -> read_from_debug_file c
      | _ -> [] in
    let () = add_entry_with_options (fun x k -> Hashtbl.add debug_map x k) xs in
    let () = add_entry_with_options (fun x k -> 
        let re = Str.regexp_case_fold x 
        in regexp_line := (re,k)::!regexp_line) !regexp_line_str in
    ()

  let ho_aux ?(call_site="") ?(arg_rgx=None) df lz (loop_d:bool) (test:'z -> bool) (g:('a->'z) option) (s:string) (args:string list) (pr_o:'z->string) (f:'a->'z) (e:'a) :'z =
    (* let call_site, _ = VarGen.last_posn # get_rm in *)
    (* let call_site, call_name = VarGen.last_posn # get in *)
    (* let call_site =                                      *)
    (*   if String.compare call_name s = 0 then             *)
    (*     let () = VarGen.last_posn # reset in             *)
    (*     call_site                                        *)
    (*   else ""                                            *)
    (* in                                                   *)
    let call_site = 
      if call_site="" then VarGen.last_posn # get_rm s 
      else call_site in
    let pre_str = "(=="^call_site^"==)" in
    (* if s=="" thenmatch s with  *)
    (*   | None -> "" *)
    (*   | Some s ->  *)
    (*         (\* let () = VarGen.last_posn := None in *\) *)
    (*         "("^s^")" in *)
    let pr_args xs =
      let rec helper (i:int) args = match args with
        | [] -> ()
        | a::args -> (print_string (s^" inp"^(string_of_int i)^" :"^a^"\n");(helper (i+1) args)) in
      helper 1 xs in
    let pr_lazy_res xs =
      let rec helper xs = match xs with
        | [] -> ()
        | (i,a)::xs -> let a1=Lazy.force a in
          if (a1=(List.nth args (i-1))) then helper xs
          else (print_string (s^" res"^(string_of_int i)^" :"^(a1)^"\n");(helper xs)) in
      helper xs in
    let check_args args = true
    (* match !z_debug_arg with *)
    (*   | None -> false  *)
    (*   | Some re ->  *)
    (*         (\* let () = print_endline ("check_args:"^s) in *\) *)
    (*         List.exists (fun x ->  *)
    (*             try  *)
    (*               (Str.search_forward re x 0);true *)
    (*             with _ -> false) args *)
    in
    let (test,pr_o) = match g with
      | None -> (test,pr_o)
      | Some g -> 
        let res = ref (None:(string option)) in
        let new_test z =
          (try
             let r = g e in
             let rs = pr_o r in              
             if String.compare (pr_o z) rs==0 then false
             else (res := Some rs; true)
           with ex ->  
             (res := Some (" OLD COPY : EXIT Exception"^(Printexc.to_string ex)^"!\n");
              true)) in
        let new_pr_o x = (match !res with
            | None -> pr_o x
            | Some s -> ("DIFFERENT RESULT from PREVIOUS METHOD"^
                         ("\n PREV :"^s)^
                         ("\n NOW :"^(pr_o x)))) in
        (new_test, new_pr_o) in
    let s,h = push_call_gen s df in
    let lc = (StackTrace.ctr # get_last_call) in
    let () = debug_calls # add_id lc in
    let h = pre_str^"\n"^h in
    let arg = if loop_d || !VarGen.trace_loop_all 
      then String.concat "  " (pick_front 80 args) else "" in
    (if loop_d || !VarGen.trace_loop 
     then print_string ("\n"^h^" ENTRY :"^arg^"\n"));
    flush stdout;
    let r = (try
               pop_aft_apply_with_exc s f e
             with ex -> 
               (
                 (* if not df then *) 
                 let flag = check_args args in
                 if flag then
                   begin
                     let () = print_string ("\n"^h^"\n") in
                     (pr_args args; pr_lazy_res lz);
                     let () = print_string (s^" EXIT Exception"^(Printexc.to_string ex)^"Occurred!\n") in
                     flush stdout;
                     raise ex 
                   end
                 else raise ex
               )) in
    (if not(test r) then r else
       (* if not df then *)
       let res_str = pr_o r in
       let flag = check_args (res_str::args) in
       if not(flag) then r
       else
         begin
           let () = print_string ("\n"^h^"\n") in
           (pr_args args; pr_lazy_res lz);
           let () = print_string (s^" EXIT:"^(res_str)^"\n") in
           flush stdout;
           r
         end
    )

  let choose bs xs = 
    let rec hp bs xs = match bs,xs with
      |[], _ -> []
      | _, [] -> []
      | b::bs, (i,s)::xs -> if b then (i,s)::(hp bs xs) else (hp bs xs) in
    hp bs xs

  let ho_aux_no s (f:'a -> 'z) (last:'a) : 'z =
    (* WN : why was this clearing done traced debug function? *)                   
    (* let ff z =  *)
    (*     let () = VarGen.last_posn # reset in *)
    (*     f z in *)
    (* let () =                                                                                       *)
    (*   let x_call_site, x_call_name = VarGen.last_posn # get in                                     *)
    (*   (* let () = print_endline ("ho_aux_no: " ^ s ^ " @" ^ x_call_site ^ ":" ^ x_call_name) in *) *)
    (*   if String.compare x_call_name s = 0 then VarGen.last_posn # reset                            *)
    (* in                                                                                             *)
    VarGen.last_posn # reset s;
    push_no_call ();
    pop_aft_apply_with_exc_no s f last


  let ho_1_opt_aux df (flags:bool list) (loop_d:bool) (test:'z -> bool) g (s:string) (pr1:'a->string) (pr_o:'z->string)  (f:'a -> 'z) (e1:'a) : 'z =
    let a1 = pr1 e1 in
    let lz = choose flags [(1,lazy (pr1 e1))] in
    let f  = f in
    ho_aux df lz loop_d test g s [a1] pr_o  f  e1


  let ho_2_opt_aux df (flags:bool list) (loop_d:bool) (test:'z -> bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr_o:'z->string)  (f:'a -> 'b -> 'z) 
      (e1:'a) (e2:'b) : 'z =
    let a1 = pr1 e1 in
    let a2 = pr2 e2 in
    let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2))] in
    let f  = f e1 in
    let g  = match g with None -> None | Some g -> Some (g e1) in
    ho_aux df lz loop_d test g s [a1;a2] pr_o f e2

  let ho_3_opt_aux df  (flags:bool list) (loop_d:bool) (test:'z -> bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr_o:'z->string)  (f:'a -> 'b -> 'c -> 'z) (e1:'a) (e2:'b) (e3:'c) : 'z =
    let a1 = pr1 e1 in
    let a2 = pr2 e2 in
    let a3 = pr3 e3 in
    let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3))] in
    let f  = f e1 e2 in
    let g  = match g with None -> None | Some g -> Some (g e1 e2) in
    ho_aux df lz loop_d test g s [a1;a2;a3] pr_o f e3


  let ho_4_opt_aux df (flags:bool list) (loop_d:bool) (test:'z->bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd-> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d): 'z =
    let call_site = VarGen.last_posn # get_rm s in
    (* let () = print_endline_quiet ("*** call_site:"^call_site^s) in *)
    let a1 = pr1 e1 in
    let a2 = pr2 e2 in
    let a3 = pr3 e3 in
    let a4 = pr4 e4 in
    let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4))] in
    let f  = f e1 e2 e3 in
    let g  = match g with None -> None | Some g -> Some (g e1 e2 e3) in
    ho_aux ~call_site:call_site df lz loop_d test g s [a1;a2;a3;a4] pr_o f e4


  let ho_5_opt_aux df (flags:bool list) (loop_d:bool) (test:'z -> bool)  g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string)
      (pr5:'e->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd -> 'e -> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d) (e5:'e) : 'z =
    let a1 = pr1 e1 in
    let a2 = pr2 e2 in
    let a3 = pr3 e3 in
    let a4 = pr4 e4 in
    let a5 = pr5 e5 in
    let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4)); (5,lazy (pr5 e5))] in
    let f  = f e1 e2 e3 e4 in
    let g  = match g with None -> None | Some g -> Some (g e1 e2 e3 e4) in
    ho_aux df lz loop_d test g s [a1;a2;a3;a4;a5] pr_o f e5


  let ho_6_opt_aux df (flags:bool list) (loop_d:bool) (test:'z->bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string)
      (pr5:'e->string) (pr6:'f->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d) (e5:'e) (e6:'f): 'z =
    let a1 = pr1 e1 in
    let a2 = pr2 e2 in
    let a3 = pr3 e3 in
    let a4 = pr4 e4 in
    let a5 = pr5 e5 in
    let a6 = pr6 e6 in
    let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4)); (5,lazy (pr5 e5)); (6,lazy (pr6 e6))] in
    let f  = f e1 e2 e3 e4 e5 in
    let g  = match g with None -> None | Some g -> Some (g e1 e2 e3 e4 e5) in
    ho_aux df lz loop_d test g s [a1;a2;a3;a4;a5;a6] pr_o f e6

  let ho_7_opt_aux df (flags:bool list) (loop_d:bool) (test:'z->bool) g (s:string) (pr1:'a->string) (pr2:'b->string) (pr3:'c->string) (pr4:'d->string)
      (pr5:'e->string) (pr6:'f->string) (pr7:'h->string) (pr_o:'z->string) 
      (f:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h-> 'z) (e1:'a) (e2:'b) (e3:'c) (e4:'d) (e5:'e) (e6:'f) (e7:'h): 'z =
    let a1 = pr1 e1 in
    let a2 = pr2 e2 in
    let a3 = pr3 e3 in
    let a4 = pr4 e4 in
    let a5 = pr5 e5 in
    let a6 = pr6 e6 in
    let a7 = pr7 e7 in
    let lz = choose flags [(1,lazy (pr1 e1)); (2,lazy (pr2 e2)); (3,lazy (pr3 e3)); (4,lazy (pr4 e4)); (5,lazy (pr5 e5)); (6,lazy (pr6 e6)); (7,lazy (pr7 e7))] in
    let f  = f e1 e2 e3 e4 e5 e6 in
    let g  = match g with None -> None | Some g -> Some (g e1 e2 e3 e4 e5 e6) in
    ho_aux df lz loop_d test g s [a1;a2;a3;a4;a5;a6;a7] pr_o f e7

  (* better re-organization *)
  (* f:output->bool, b_loop:bool *)
  let ho_1_preopt f b_loop = ho_1_opt_aux false [] b_loop f None
  let to_1_preopt f b_loop = ho_1_opt_aux true [] b_loop f None
  let ho_1_pre b_loop = ho_1_preopt (fun _ -> true) b_loop
  let to_1_pre b_loop = to_1_preopt (fun _ -> true) b_loop
  let ho_1 s = ho_1_pre false s
  let to_1 s = to_1_pre false s
  let ho_1_opt f = ho_1_preopt f false
  let ho_1_loop s = ho_1_pre true s 



  let go_1 t_flag l_flag s = ho_1_opt_aux t_flag [] l_flag (fun _ -> true) None s
  let go_2 t_flag l_flag s = ho_2_opt_aux t_flag [] l_flag (fun _ -> true) None s
  let go_3 t_flag l_flag s = ho_3_opt_aux t_flag [] l_flag (fun _ -> true) None s
  let go_4 t_flag l_flag s = ho_4_opt_aux t_flag [] l_flag (fun _ -> true) None s
  let go_5 t_flag l_flag s = ho_5_opt_aux t_flag [] l_flag (fun _ -> true) None s
  let go_6 t_flag l_flag s = ho_6_opt_aux t_flag [] l_flag (fun _ -> true) None s
  let go_7 t_flag l_flag s = ho_7_opt_aux t_flag [] l_flag (fun _ -> true) None s

  (* let ho_1 s = go_1 false false s *)
  (* let ho_2 s = go_2 false false s *)
  (* let ho_3 s = go_3 false false s *)
  (* let ho_4 s = go_4 false false s *)
  (* let ho_5 s = go_5 false false s *)
  (* let ho_6 s = go_6 false false s *)
  (* let ho_7 s = go_7 false false s *)

  (* let to_1 s = go_1 true false s *)
  (* let to_2 s = go_2 true false s *)
  (* let to_3 s = go_3 true false s *)
  (* let to_4 s = go_4 true false s *)
  (* let to_5 s = go_5 true false s *)
  (* let to_6 s = go_6 true false s *)
  (* let to_7 s = go_7 true false s *)

  (* let ho_1_loop s = go_1 false true s *)
  (* let ho_2_loop s = go_2 false true s *)
  (* let ho_3_loop s = go_3 false true s *)
  (* let ho_4_loop s = go_4 false true s *)
  (* let ho_5_loop s = go_5 false true s *)
  (* let ho_6_loop s = go_6 false true s *)
  (* let ho_7_loop s = go_7 false true s *)

  (* let ho_1 s = ho_1_opt_aux false [] false (fun _ -> true) None s *)
  (* let ho_2 s = ho_2_opt_aux false [] false (fun _ -> true) None s *)
  (* let ho_3 s = ho_3_opt_aux false [] false (fun _ -> true) None s *)
  (* let ho_4 s = ho_4_opt_aux false [] false (fun _ -> true) None s *)
  (* let ho_5 s = ho_5_opt_aux false [] false (fun _ -> true) None s *)
  (* let ho_6 s = ho_6_opt_aux false [] false (fun _ -> true) None s *)
  (* let ho_7 s = ho_7_opt_aux false [] false (fun _ -> true) None s *)

  (* let to_1 s = ho_1_opt_aux true [] false (fun _ -> true) None s *)
  (* let to_2 s = ho_2_opt_aux true [] false (fun _ -> true) None s *)
  (* let to_3 s = ho_3_opt_aux true [] false (fun _ -> true) None s *)
  (* let to_4 s = ho_4_opt_aux true [] false (fun _ -> true) None s *)
  (* let to_5 s = ho_5_opt_aux true [] false (fun _ -> true) None s *)
  (* let to_6 s = ho_6_opt_aux true [] false (fun _ -> true) None s *)
  (* let to_7 s = ho_7_opt_aux true [] false (fun _ -> true) None s *)

  (* let ho_1_loop s = ho_1_opt_aux false [] true (fun _ -> true) None s *)
  (* let ho_2_loop s = ho_2_opt_aux false [] true (fun _ -> true) None s *)
  (* let ho_3_loop s = ho_3_opt_aux false [] true (fun _ -> true) None s *)
  (* let ho_4_loop s = ho_4_opt_aux false [] true (fun _ -> true) None s *)
  (* let ho_5_loop s = ho_5_opt_aux false [] true (fun _ -> true) None s *)
  (* let ho_6_loop s = ho_6_opt_aux false [] true (fun _ -> true) None s *)
  (* let ho_7_loop s = ho_7_opt_aux false [] true (fun _ -> true) None s *)

  let wrap_pop_call f x =
    try
      let r = f x in
      let () = debug_calls # pop_call in
      r
    with e -> 
      let () = debug_calls # pop_call in
      raise e

  let splitter s f_none f_gen f_norm =
    (* VarGen.last_posn # get --> The call site (with x_add) of s *)
    (* let x_call_site, x_call_name = VarGen.last_posn # get in                                    *)
    (* let () = if x_call_name = "" then VarGen.last_posn # replace (x_call_site, s) in            *)
    (* let at = if x_call_site = "" || not (String.compare x_call_name s = 0) then "" else " @" in *)
    let () = VarGen.last_posn # set_name s in
    let at_call_site =
      let x_call_site = VarGen.last_posn # get s in
      if x_call_site = "" then ""
      else " @" ^ x_call_site
    in
    let () = if !dump_callers_flag (* z_debug_flag *) then debug_calls # push_call s at_call_site in
    let () = if !dump_calls then debug_calls # print_call s at_call_site in
    let fn = if !z_debug_flag then
        match (in_debug s) with
        | DO_Normal -> f_gen (f_norm false false)
        | DO_Trace -> f_gen (f_norm true false) 
        | DO_Loop -> f_gen (f_norm false true)
        | DO_Both -> f_gen (f_norm true true)
        | DO_None -> 
          (* let _ = print_endline ("splitter(none):"^s) in  *)
          f_none
      else         
        (* let _ = print_endline ("splitter(none):"^s) in  *)
        f_none in 
    if !dump_calls then wrap_pop_call fn
    else fn


  let no_1 s p1 p0 f =
    let code_gen fn = fn s p1 p0 f in
    let code_none = ho_aux_no s f in
    splitter s code_none code_gen go_1 

  let no_2 s p1 p2 p0 f e1 =
    let code_gen fn = fn s p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen go_2

  let no_3 s p1 p2 p3 p0 f e1 e2 =
    let code_gen fn = fn s p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen go_3

  let no_4 s p1 p2 p3 p4 p0 f e1 e2 e3 =
    (* let call_site = VarGen.last_posn # get_rm s in *)
    let code_gen fn = fn s p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no (* ~call_site:call_site *) s (f e1 e2 e3) in
    splitter s code_none code_gen go_4

  let no_5 s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let code_gen fn = fn s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen go_5

  let no_6 s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let code_gen fn = fn s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen go_6

  let no_7 s p1 p2 p3 p4 p5 p6 p7 p0 f e1 e2 e3 e4 e5 e6 =
    let code_gen fn = fn s p1 p2 p3 p4 p5 p6 p7 p0 f e1 e2 e3 e4 e5 e6 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5 e6) in
    splitter s code_none code_gen go_7


  let ho_1_opt tr_flag lp_flag f = ho_1_opt_aux tr_flag [] lp_flag f None
  let ho_2_opt tr_flag lp_flag f = ho_2_opt_aux tr_flag [] lp_flag f None
  let ho_3_opt tr_flag lp_flag f = ho_3_opt_aux tr_flag [] lp_flag f None
  let ho_4_opt tr_flag lp_flag f = ho_4_opt_aux tr_flag [] lp_flag f None
  let ho_5_opt tr_flag lp_flag f = ho_5_opt_aux tr_flag [] lp_flag f None
  let ho_6_opt tr_flag lp_flag f = ho_6_opt_aux tr_flag [] lp_flag f None

  (* let ho_1_opt f = ho_1_opt_aux false [] false f None *)
  (* let ho_2_opt f = ho_2_opt_aux false [] false f None *)
  (* let ho_3_opt f = ho_3_opt_aux false [] false f None *)
  (* let ho_4_opt f = ho_4_opt_aux false [] false f None *)
  (* let ho_5_opt f = ho_5_opt_aux false [] false f None *)
  (* let ho_6_opt f = ho_6_opt_aux false [] false f None *)

  (* let to_1_opt f = ho_1_opt_aux true [] false f None *)
  (* let to_2_opt f = ho_2_opt_aux true [] false f None *)
  (* let to_3_opt f = ho_3_opt_aux true [] false f None *)
  (* let to_4_opt f = ho_4_opt_aux true [] false f None *)
  (* let to_5_opt f = ho_5_opt_aux true [] false f None *)
  (* let to_6_opt f = ho_6_opt_aux true [] false f None *)
  (* let to_7_opt f = ho_7_opt_aux true [] false f None *)

  (* let no_1_opt _ _ _ _ f  *)
  (*       = ho_aux_no f *)
  (* let no_2_opt _ _ _ _ _ f e1  *)
  (*       = ho_aux_no (f e1) *)
  (* let no_3_opt _ _ _ _ _ _ f e1 e2  *)
  (*       = ho_aux_no (f e1 e2) *)
  (* let no_4_opt _ _ _ _ _ _ _ f e1 e2 e3  *)
  (*       = ho_aux_no (f e1 e2 e3) *)
  (* let no_5_opt _ _ _ _ _ _ _ _ f e1 e2 e3 e4  *)
  (*       = ho_aux_no (f e1 e2 e3 e4) *)
  (* let no_6_opt _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5  *)
  (*       = ho_aux_no (f e1 e2 e3 e4 e5) *)

  let no_1_opt op s p1 p0 f =
    let code_gen fn = fn op s p1 p0 f in
    let code_none = ho_aux_no s (f) in
    splitter s code_none code_gen ho_1_opt

  let no_2_opt op s p1 p2 p0 f e1 =
    let code_gen fn = fn op s p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen ho_2_opt

  let no_3_opt op s p1 p2 p3 p0 f e1 e2 =
    let code_gen fn = fn op s p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen ho_3_opt

  let no_4_opt op s p1 p2 p3 p4 p0 f e1 e2 e3 =
    let code_gen fn = fn op s p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no s (f e1 e2 e3) in
    splitter s code_none code_gen ho_4_opt

  let no_5_opt op s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let code_gen fn = fn op s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen ho_5_opt

  let no_6_opt op s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let code_gen fn = fn op s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen ho_6_opt

  let add_num f i s = let str=(s^"#"^(string_of_int i)) in f str

  let go_1_num tr_flag lp_flag i =  add_num (go_1 tr_flag lp_flag) i
  let go_2_num tr_flag lp_flag i =  add_num (go_2 tr_flag lp_flag) i
  let go_3_num tr_flag lp_flag i =  add_num (go_3 tr_flag lp_flag) i
  let go_4_num tr_flag lp_flag i =  add_num (go_4 tr_flag lp_flag) i
  let go_5_num tr_flag lp_flag i =  add_num (go_5 tr_flag lp_flag) i
  let go_6_num tr_flag lp_flag i =  add_num (go_6 tr_flag lp_flag) i

  let add_num_opt f i p s = let str=(s^"#"^(string_of_int i)) in f p str

  let go_1_num_opt tr_flag lp_flag i =  add_num_opt (ho_1_opt tr_flag lp_flag) i
  let go_2_num_opt tr_flag lp_flag i =  add_num_opt (ho_2_opt tr_flag lp_flag) i
  let go_3_num_opt tr_flag lp_flag i =  add_num_opt (ho_3_opt tr_flag lp_flag) i
  let go_4_num_opt tr_flag lp_flag i =  add_num_opt (ho_4_opt tr_flag lp_flag) i
  let go_5_num_opt tr_flag lp_flag i =  add_num_opt (ho_5_opt tr_flag lp_flag) i
  let go_6_num_opt tr_flag lp_flag i =  add_num_opt (ho_6_opt tr_flag lp_flag) i

  (* let ho_1_num i =  add_num ho_1 i *)
  (* let ho_2_num i =  add_num ho_2 i *)
  (* let ho_3_num i =  add_num ho_3 i *)
  (* let ho_4_num i =  add_num ho_4 i *)
  (* let ho_5_num i =  add_num ho_5 i *)
  (* let ho_6_num i =  add_num ho_6 i *)

  (* let to_1_num i =  add_num to_1 i *)
  (* let to_2_num i =  add_num to_2 i *)
  (* let to_3_num i =  add_num to_3 i *)
  (* let to_4_num i =  add_num to_4 i *)
  (* let to_5_num i =  add_num to_5 i *)
  (* let to_6_num i =  add_num to_6 i *)

  (* let ho_1_loop_num i =  add_num ho_1_loop i *)
  (* let ho_2_loop_num i =  add_num ho_2_loop i *)
  (* let ho_3_loop_num i =  add_num ho_3_loop i *)
  (* let ho_4_loop_num i =  add_num ho_4_loop i *)
  (* let ho_5_loop_num i =  add_num ho_5_loop i *)
  (* let ho_6_loop_num i =  add_num ho_6_loop i *)

  let to_1_loop s = ho_1_opt_aux true [] true (fun _ -> true) None s
  let to_2_loop s = ho_2_opt_aux true [] true (fun _ -> true) None s
  let to_3_loop s = ho_3_opt_aux true [] true (fun _ -> true) None s
  let to_4_loop s = ho_4_opt_aux true [] true (fun _ -> true) None s
  let to_5_loop s = ho_5_opt_aux true [] true (fun _ -> true) None s
  let to_6_loop s = ho_6_opt_aux true [] true (fun _ -> true) None s

  let to_1_loop_num i =  add_num to_1_loop i
  let to_2_loop_num i =  add_num to_2_loop i
  let to_3_loop_num i =  add_num to_3_loop i
  let to_4_loop_num i =  add_num to_4_loop i
  let to_5_loop_num i =  add_num to_5_loop i
  let to_6_loop_num i =  add_num to_6_loop i

  (* let no_1_num (i:int) s _ _ f *)
  (*       = ho_aux_no f *)
  (* let no_2_num (i:int) s _ _ _ f e1 *)
  (*       = ho_aux_no (f e1) *)
  (* let no_3_num (i:int) s _ _ _ _ f e1 e2 *)
  (*       = ho_aux_no (f e1 e2) *)
  (* let no_4_num (i:int) s _ _ _ _ _ f e1 e2 e3 *)
  (*       = ho_aux_no (f e1 e2 e3) *)
  (* let no_5_num (i:int) s _ _ _ _ _ _ f e1 e2 e3 e4 *)
  (*       = ho_aux_no (f e1 e2 e3 e4) *)
  (* let no_6_num (i:int) s _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 *)
  (*       = ho_aux_no (f e1 e2 e3 e4 e5) *)

  let no_1_num_opt (i:int) p s p1 p0 f =
    let code_gen fn = fn i p s p1 p0 f in
    let code_none = ho_aux_no s f in
    splitter s code_none code_gen go_1_num_opt 

  let no_2_num_opt (i:int) p s p1 p2 p0 f e1 =
    let code_gen fn = fn i p s p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen go_2_num_opt 

  let no_3_num_opt (i:int) p s p1 p2 p3 p0 f e1 e2 =
    let code_gen fn = fn i p s p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen go_3_num_opt 

  let no_4_num_opt (i:int) p s p1 p2 p3 p4 p0 f e1 e2 e3 =
    let code_gen fn = fn i p s p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no s (f e1 e2 e3) in
    splitter s code_none code_gen go_4_num_opt 

  let no_5_num_opt (i:int) p s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let code_gen fn = fn i p s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen go_5_num_opt 

  let no_6_num_opt (i:int) p s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let code_gen fn = fn i p s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen go_6_num_opt 

  let no_1_num (i:int) s p1 p0 f =
    let code_gen fn = fn i s p1 p0 f in
    let code_none = ho_aux_no s f in
    splitter s code_none code_gen go_1_num 

  let no_2_num (i:int) s p1 p2 p0 f e1 =
    let code_gen fn = fn i s p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen go_2_num 

  let no_3_num (i:int) s p1 p2 p3 p0 f e1 e2 =
    let code_gen fn = fn i s p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen go_3_num 

  let no_4_num (i:int) s p1 p2 p3 p4 p0 f e1 e2 e3 =
    let code_gen fn = fn i s p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no s (f e1 e2 e3) in
    splitter s code_none code_gen go_4_num 

  let no_5_num (i:int) s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let code_gen fn = fn i s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen go_5_num 

  let no_6_num (i:int) s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let code_gen fn = fn i s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen go_6_num 

  let ho_1_cmp tr_flag lp_flag g = ho_1_opt_aux tr_flag [] lp_flag (fun _ -> true) (Some g) 
  let ho_2_cmp tr_flag lp_flag g = ho_2_opt_aux tr_flag [] lp_flag (fun _ -> true) (Some g) 
  let ho_3_cmp tr_flag lp_flag g = ho_3_opt_aux tr_flag [] lp_flag (fun _ -> true) (Some g) 
  let ho_4_cmp tr_flag lp_flag g = ho_4_opt_aux tr_flag [] lp_flag (fun _ -> true) (Some g) 
  let ho_5_cmp tr_flag lp_flag g = ho_5_opt_aux tr_flag [] lp_flag (fun _ -> true) (Some g) 
  let ho_6_cmp tr_flag lp_flag g = ho_6_opt_aux tr_flag [] lp_flag (fun _ -> true) (Some g) 

  (* let ho_1_cmp g = ho_1_opt_aux false [] false (fun _ -> true) (Some g)  *)
  (* let ho_2_cmp g = ho_2_opt_aux false [] false (fun _ -> true) (Some g)  *)
  (* let ho_3_cmp g = ho_3_opt_aux false [] false (fun _ -> true) (Some g)  *)
  (* let ho_4_cmp g = ho_4_opt_aux false [] false (fun _ -> true) (Some g)  *)
  (* let ho_5_cmp g = ho_5_opt_aux false [] false (fun _ -> true) (Some g)  *)
  (* let ho_6_cmp g = ho_6_opt_aux false [] false (fun _ -> true) (Some g)  *)

  (* let to_1_cmp g = ho_1_opt_aux true [] false (fun _ -> true) (Some g)  *)
  (* let to_2_cmp g = ho_2_opt_aux true [] false (fun _ -> true) (Some g)  *)
  (* let to_3_cmp g = ho_3_opt_aux true [] false (fun _ -> true) (Some g)  *)
  (* let to_4_cmp g = ho_4_opt_aux true [] false (fun _ -> true) (Some g)  *)
  (* let to_5_cmp g = ho_5_opt_aux true [] false (fun _ -> true) (Some g)  *)
  (* let to_6_cmp g = ho_6_opt_aux true [] false (fun _ -> true) (Some g)  *)

  (* let ho_1_cmp_loop g = ho_1_opt_aux false [] true (fun _ -> true) (Some g)  *)
  (* let ho_2_cmp_loop g = ho_2_opt_aux false [] true (fun _ -> true) (Some g)  *)
  (* let ho_3_cmp_loop g = ho_3_opt_aux false [] true (fun _ -> true) (Some g)  *)
  (* let ho_4_cmp_loop g = ho_4_opt_aux false [] true (fun _ -> true) (Some g)  *)
  (* let ho_5_cmp_loop g = ho_5_opt_aux false [] true (fun _ -> true) (Some g)  *)
  (* let ho_6_cmp_loop g = ho_6_opt_aux false [] true (fun _ -> true) (Some g)  *)

  let no_1_cmp g s p1 p0 f =
    let code_gen fn = fn g s p1 p0 f in
    let code_none = ho_aux_no s f in
    splitter s code_none code_gen ho_1_cmp 

  let no_2_cmp g s p1 p2 p0 f e1 =
    let code_gen fn = fn g s p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen ho_2_cmp 

  let no_3_cmp g s p1 p2 p3 p0 f e1 e2 =
    let code_gen fn = fn g s p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen ho_3_cmp 

  let no_4_cmp g s p1 p2 p3 p4 p0 f e1 e2 e3 =
    let code_gen fn = fn g s p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no s (f e1 e2 e3) in
    splitter s code_none code_gen ho_4_cmp 

  let no_5_cmp g s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let code_gen fn = fn g s p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen ho_5_cmp 

  let no_6_cmp g s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let code_gen fn = fn g s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen ho_6_cmp 

  (* let no_6_cmp g s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 = *)
  (*   let code_gen fn = fn g s p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in *)
  (*   let code_none = ho_aux_no s (f e1 e2 e3 e4 p5) in *)
  (*   splitter s code_none code_gen ho_6_cmp to_6_cmp ho_6_cmp_loop *)

  (* let no_1_cmp _ _ _ _ f  *)
  (*       = ho_aux_no s f *)
  (* let no_2_cmp _ _ _ _ _ f e1  *)
  (*       = ho_aux_no s (f e1) *)
  (* let no_3_cmp _ _ _ _ _ _ f e1 e2  *)
  (*       = ho_aux_no s (f e1 e2) *)
  (* let no_4_cmp _ _ _ _ _ _ _ f e1 e2 e3  *)
  (*       = ho_aux_no s (f e1 e2 e3) *)
  (* let no_5_cmp _ _ _ _ _ _ _ _ f e1 e2 e3 e4  *)
  (*       = ho_aux_no s (f e1 e2 e3 e4) *)
  (* let no_6_cmp _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5  *)
  (*       = ho_aux_no s (f e1 e2 e3 e4 e5) *)

  let ho_eff_1 tr_flag lp_flag s l = ho_1_opt_aux tr_flag l lp_flag (fun _ -> true) None s
  let ho_eff_2 tr_flag lp_flag s l = ho_2_opt_aux tr_flag l lp_flag (fun _ -> true) None s
  let ho_eff_3 tr_flag lp_flag s l = ho_3_opt_aux tr_flag l lp_flag (fun _ -> true) None s
  let ho_eff_4 tr_flag lp_flag s l = ho_4_opt_aux tr_flag l lp_flag (fun _ -> true) None s
  let ho_eff_5 tr_flag lp_flag s l = ho_5_opt_aux tr_flag l lp_flag (fun _ -> true) None s
  let ho_eff_6 tr_flag lp_flag s l = ho_6_opt_aux tr_flag l lp_flag (fun _ -> true) None s

  (* let ho_eff_1 s l = ho_1_opt_aux false l false (fun _ -> true) None s *)
  (* let ho_eff_2 s l = ho_2_opt_aux false l false (fun _ -> true) None s *)
  (* let ho_eff_3 s l = ho_3_opt_aux false l false (fun _ -> true) None s *)
  (* let ho_eff_4 s l = ho_4_opt_aux false l false (fun _ -> true) None s *)
  (* let ho_eff_5 s l = ho_5_opt_aux false l false (fun _ -> true) None s *)
  (* let ho_eff_6 s l = ho_6_opt_aux false l false (fun _ -> true) None s *)

  (* let ho_eff_1_loop s l = ho_1_opt_aux false l true (fun _ -> true) None s *)
  (* let ho_eff_2_loop s l = ho_2_opt_aux false l true (fun _ -> true) None s *)
  (* let ho_eff_3_loop s l = ho_3_opt_aux false l true (fun _ -> true) None s *)
  (* let ho_eff_4_loop s l = ho_4_opt_aux false l true (fun _ -> true) None s *)
  (* let ho_eff_5_loop s l = ho_5_opt_aux false l true (fun _ -> true) None s *)
  (* let ho_eff_6_loop s l = ho_6_opt_aux false l true (fun _ -> true) None s *)

  (* let to_eff_1 s l = ho_1_opt_aux true l false (fun _ -> true) None s *)
  (* let to_eff_2 s l = ho_2_opt_aux true l false (fun _ -> true) None s *)
  (* let to_eff_3 s l = ho_3_opt_aux true l false (fun _ -> true) None s *)
  (* let to_eff_4 s l = ho_4_opt_aux true l false (fun _ -> true) None s *)
  (* let to_eff_5 s l = ho_5_opt_aux true l false (fun _ -> true) None s *)
  (* let to_eff_6 s l = ho_6_opt_aux true l false (fun _ -> true) None s *)

  (* let no_eff_1 _ _ _ _ f  *)
  (*       = ho_aux_no s f *)

  let no_eff_1 s l p1 p0 f =
    let code_gen fn = fn s l p1 p0 f in
    let code_none = ho_aux_no s f in
    splitter s code_none code_gen ho_eff_1 

  let no_eff_2 s l p1 p2 p0 f e1 =
    let code_gen fn = fn s l p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen ho_eff_2 

  let no_eff_3 s l p1 p2 p3 p0 f e1 e2 =
    let code_gen fn = fn s l p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen ho_eff_3 

  let no_eff_4 s l p1 p2 p3 p4 p0 f e1 e2 e3 =
    let code_gen fn = fn s l p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no s (f e1 e2 e3) in
    splitter s code_none code_gen ho_eff_4 

  let no_eff_5 s l p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let code_gen fn = fn s l p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen ho_eff_5 

  let no_eff_6 s l p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let code_gen fn = fn s l p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen ho_eff_6 

  (* let no_eff_2 _ _ _ _ _ f e1  *)
  (*       = ho_aux_no (f e1) *)
  (* let no_eff_3 _ _ _ _ _ _ f e1 e2  *)
  (*       = ho_aux_no (f e1 e2) *)
  (* let no_eff_4 _ _ _ _ _ _ _ f e1 e2 e3  *)
  (*       = ho_aux_no (f e1 e2 e3) *)
  (* let no_eff_5 _ _ _ _ _ _ _ _ f e1 e2 e3 e4  *)
  (*       = ho_aux_no (f e1 e2 e3 e4) *)
  (* let no_eff_6 _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5  *)
  (*       = ho_aux_no (f e1 e2 e3 e4 e5) *)

  let ho_eff_1_num tr_flag lp_flag i =  add_num (ho_eff_1 tr_flag lp_flag) i
  let ho_eff_2_num tr_flag lp_flag i =  add_num (ho_eff_2 tr_flag lp_flag) i
  let ho_eff_3_num tr_flag lp_flag i =  add_num (ho_eff_3 tr_flag lp_flag) i
  let ho_eff_4_num tr_flag lp_flag i =  add_num (ho_eff_4 tr_flag lp_flag) i
  let ho_eff_5_num tr_flag lp_flag i =  add_num (ho_eff_5 tr_flag lp_flag) i
  let ho_eff_6_num tr_flag lp_flag i =  add_num (ho_eff_6 tr_flag lp_flag) i

  (* let ho_eff_1_num_loop i =  add_num ho_eff_1_loop i *)
  (* let ho_eff_2_num_loop i =  add_num ho_eff_2_loop i *)
  (* let ho_eff_3_num_loop i =  add_num ho_eff_3_loop i *)
  (* let ho_eff_4_num_loop i =  add_num ho_eff_4_loop i *)
  (* let ho_eff_5_num_loop i =  add_num ho_eff_5_loop i *)
  (* let ho_eff_6_num_loop i =  add_num ho_eff_6_loop i *)

  (* let ho_eff_1_num i =  add_num ho_eff_1 i *)
  (* let ho_eff_2_num i =  add_num ho_eff_2 i *)
  (* let ho_eff_3_num i =  add_num ho_eff_3 i *)
  (* let ho_eff_4_num i =  add_num ho_eff_4 i *)
  (* let ho_eff_5_num i =  add_num ho_eff_5 i *)
  (* let ho_eff_6_num i =  add_num ho_eff_6 i *)

  (* let to_eff_1_num i =  add_num to_eff_1 i *)
  (* let to_eff_2_num i =  add_num to_eff_2 i *)
  (* let to_eff_3_num i =  add_num to_eff_3 i *)
  (* let to_eff_4_num i =  add_num to_eff_4 i *)
  (* let to_eff_5_num i =  add_num to_eff_5 i *)
  (* let to_eff_6_num i =  add_num to_eff_6 i *)

  let no_eff_1_num i s l p1 p0 f =
    let code_gen fn = fn i s l p1 p0 f in
    let code_none = ho_aux_no s f in
    splitter s code_none code_gen ho_eff_1_num 

  let no_eff_2_num i s l p1 p2 p0 f e1 =
    let code_gen fn = fn i s l p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen ho_eff_2_num 

  let no_eff_3_num i s l p1 p2 p3 p0 f e1 e2 =
    let code_gen fn = fn i s l p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen ho_eff_3_num 

  let no_eff_4_num i s l p1 p2 p3 p4 p0 f e1 e2 e3 =
    let code_gen fn = fn i s l p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no s (f e1 e2 e3) in
    splitter s code_none code_gen ho_eff_4_num 

  let no_eff_5_num i s l p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let code_gen fn = fn i s l p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen ho_eff_5_num 

  let no_eff_6_num i s l p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let code_gen fn = fn i s l p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen ho_eff_6_num 

  let ho_1_all tr_flag lp_flag s pf gf l = ho_1_opt_aux tr_flag l lp_flag pf gf s
  let ho_2_all tr_flag lp_flag s pf gf l = ho_2_opt_aux tr_flag l lp_flag pf gf s
  let ho_3_all tr_flag lp_flag s pf gf l = ho_3_opt_aux tr_flag l lp_flag pf gf s
  let ho_4_all tr_flag lp_flag s pf gf l = ho_4_opt_aux tr_flag l lp_flag pf gf s
  let ho_5_all tr_flag lp_flag s pf gf l = ho_5_opt_aux tr_flag l lp_flag pf gf s
  let ho_6_all tr_flag lp_flag s pf gf l = ho_6_opt_aux tr_flag l lp_flag pf gf s

  let ho_1_all_num tr_flag lp_flag i =  add_num (ho_1_all tr_flag lp_flag) i
  let ho_2_all_num tr_flag lp_flag i =  add_num (ho_2_all tr_flag lp_flag) i
  let ho_3_all_num tr_flag lp_flag i =  add_num (ho_3_all tr_flag lp_flag) i
  let ho_4_all_num tr_flag lp_flag i =  add_num (ho_4_all tr_flag lp_flag) i
  let ho_5_all_num tr_flag lp_flag i =  add_num (ho_5_all tr_flag lp_flag) i
  let ho_6_all_num tr_flag lp_flag i =  add_num (ho_6_all tr_flag lp_flag) i

  let no_1_all i s pf gf l p1 p0 f =
    let pf = match pf with 
        Some p -> p 
      | _ -> fun _ -> true in
    let code_gen fn = fn i s pf gf l p1 p0 f in
    let code_none = ho_aux_no s (f) in
    splitter s code_none code_gen ho_1_all_num 

  let no_2_all i s pf gf l p1 p2 p0 f e1 =
    let pf = match pf with 
        Some p -> p 
      | _ -> fun _ -> true in
    let code_gen fn = fn i s pf gf l p1 p2 p0 f e1 in
    let code_none = ho_aux_no s (f e1) in
    splitter s code_none code_gen ho_2_all_num 

  let no_3_all i s pf gf l p1 p2 p3 p0 f e1 e2 =
    let pf = match pf with 
        Some p -> p 
      | _ -> fun _ -> true in
    let code_gen fn = fn i s pf gf l p1 p2 p3 p0 f e1 e2 in
    let code_none = ho_aux_no s (f e1 e2) in
    splitter s code_none code_gen ho_3_all_num 

  let no_4_all i s pf gf l p1 p2 p3 p4 p0 f e1 e2 e3 =
    let pf = match pf with 
        Some p -> p 
      | _ -> fun _ -> true in
    let code_gen fn = fn i s pf gf l p1 p2 p3 p4 p0 f e1 e2 e3 in
    let code_none = ho_aux_no s (f e1 e2 e3) in
    splitter s code_none code_gen ho_4_all_num 

  let no_5_all i s pf gf l p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 =
    let pf = match pf with 
        Some p -> p 
      | _ -> fun _ -> true in
    let code_gen fn = fn i s pf gf l p1 p2 p3 p4 p5 p0 f e1 e2 e3 e4 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4) in
    splitter s code_none code_gen ho_5_all_num 

  let no_6_all i s pf gf l p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 =
    let pf = match pf with 
        Some p -> p 
      | _ -> fun _ -> true in
    let code_gen fn = fn i s pf gf l p1 p2 p3 p4 p5 p6 p0 f e1 e2 e3 e4 e5 in
    let code_none = ho_aux_no s (f e1 e2 e3 e4 e5) in
    splitter s code_none code_gen ho_6_all_num 

  (* let no_eff_1_num _ _ _ _ _ f  *)
  (*       =  ho_aux_no (f) *)
  (* let no_eff_2_num _ _ _ _ _ _ f e1  *)
  (*       =  ho_aux_no (f e1) *)
  (* let no_eff_3_num _ _ _ _ _ _ _ f e1 e2  *)
  (*       =  ho_aux_no (f e1 e2) *)
  (* let no_eff_4_num _ _ _ _ _ _ _ _ f e1 e2 e3  *)
  (*       =  ho_aux_no (f e1 e2 e3) *)
  (* let no_eff_5_num _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4  *)
  (*       =  ho_aux_no (f e1 e2 e3 e4) *)
  (* let no_eff_6_num _ _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5  *)
  (*       =  ho_aux_no (f e1 e2 e3 e4 e5) *)

  (* let no_1_loop _ _ _ f  *)
  (*       = ho_aux_no f *)
  (* let no_2_loop _ _ _ _ f e1  *)
  (*       = ho_aux_no (f e1) *)
  (* let no_3_loop _ _ _ _ _ f e1 e2 *)
  (*       = ho_aux_no (f e1 e2) *)
  (* let no_4_loop _ _ _ _ _ _ f e1 e2 e3 *)
  (*       = ho_aux_no (f e1 e2 e3) *)
  (* let no_5_loop _ _ _ _ _ _ _ f e1 e2 e3 e4 *)
  (*       = ho_aux_no (f e1 e2 e3 e4) *)
  (* let no_6_loop _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 *)
  (*       = ho_aux_no (f e1 e2 e3 e4 e5) *)


  (* let no_1_loop_num _ _ _ _ f  *)
  (*       = ho_aux_no f *)
  (* let no_2_loop_num _ _ _ _ _ f e1  *)
  (*       = ho_aux_no (f e1) *)
  (* let no_3_loop_num _ _ _ _ _ _ f e1 e2 *)
  (*       = ho_aux_no (f e1 e2) *)
  (* let no_4_loop_num _ _ _ _ _ _ _ f e1 e2 e3 *)
  (*       = ho_aux_no (f e1 e2 e3) *)
  (* let no_5_loop_num _ _ _ _ _ _ _ _ f e1 e2 e3 e4 *)
  (*       = ho_aux_no (f e1 e2 e3 e4) *)
  (* let no_6_loop_num _ _ _ _ _ _ _ _ _ f e1 e2 e3 e4 e5 *)
  (*       = ho_aux_no (f e1 e2 e3 e4 e5) *)

  (* let no_eff_1_opt  _ _ _ _ _ f = f *)
  (* let no_eff_2_opt  _ _ _ _ _ _ f = f *)
  (* let no_eff_3_opt  _ _ _ _ _ _ _ f = f *)
  (* let no_eff_4_opt  _ _ _ _ _ _ _ _ f = f *)
  (* let no_eff_5_opt  _ _ _ _ _ _ _ _ _ f = f *)
  (* let no_eff_6_opt  _ _ _ _ _ _ _ _ _ _ f = f *)

end


module DebugEmpty  =
struct

  let z_debug_file = ref ""
  (* let z_debug_regexp = ref None *)
  (* let z_debug_flag = ref false *)
  (* let z_debug_regexp = ref None *)
  let mk_debug_arg s =
    let re = Str.regexp s in
    z_debug_arg := Some re

  let read_main() = ()
  let dump_debug_calls () = ()
  let no_1 s p1 p0 f = f
  let no_2 s p1 p2 p0 f = f
  let no_3 s p1 p2 p3 p0 f = f
  let no_4 s p1 p2 p3 p4 p0 f = f
  let no_5 s p1 p2 p3 p4 p5 p0 f = f
  let no_6 s p1 p2 p3 p4 p5 p6 p0 f = f
  let no_7 s p1 p2 p3 p4 p5 p6 p7 p0 f = f

  let no_1_cmp g s p1 p0 f = f
  let no_2_cmp g s p1 p2 p0 f = f
  let no_3_cmp g s p1 p2 p3 p0 f = f
  let no_4_cmp g s p1 p2 p3 p4 p0 f = f
  let no_5_cmp g s p1 p2 p3 p4 p5 p0 f = f
  let no_6_cmp g s p1 p2 p3 p4 p5 p6 p0 f = f

  let no_1_opt op s p1 p0 f = f
  let no_2_opt op s p1 p2 p0 f = f
  let no_3_opt op s p1 p2 p3 p0 f = f
  let no_4_opt op s p1 p2 p3 p4 p0 f = f
  let no_5_opt op s p1 p2 p3 p4 p5 p0 f = f
  let no_6_opt op s p1 p2 p3 p4 p5 p6 p0 f = f

  let no_eff_1 s l p1 p0 f = f
  let no_eff_2 s l p1 p2 p0 f = f
  let no_eff_3 s l p1 p2 p3 p0 f = f
  let no_eff_4 s l p1 p2 p3 p4 p0 f = f
  let no_eff_5 s l p1 p2 p3 p4 p5 p0 f = f
  let no_eff_6 s l p1 p2 p3 p4 p5 p6 p0 f = f

  let no_eff_1_num i s l p1 p0 f = f
  let no_eff_2_num i s l p1 p2 p0 f = f
  let no_eff_3_num i s l p1 p2 p3 p0 f = f
  let no_eff_4_num i s l p1 p2 p3 p4 p0 f = f
  let no_eff_5_num i s l p1 p2 p3 p4 p5 p0 f = f
  let no_eff_6_num i s l p1 p2 p3 p4 p5 p6 p0 f = f

  let no_1_num (i:int) s p1 p0 f  = f
  let no_2_num (i:int) s p1 p2 p0 f  = f
  let no_3_num (i:int) s p1 p2 p3 p0 f  = f
  let no_4_num (i:int) s p1 p2 p3 p4 p0 f  = f
  let no_5_num (i:int) s p1 p2 p3 p4 p5 p0 f  = f
  let no_6_num (i:int) s p1 p2 p3 p4 p5 p6 p0 f  = f 

  let no_1_num_opt (i:int) p s p1 p0 f =  f
  let no_2_num_opt (i:int) p s p1 p2 p0 f  = f
  let no_3_num_opt (i:int) p s p1 p2 p3 p0 f = f
  let no_4_num_opt (i:int) p s p1 p2 p3 p4 p0 f = f
  let no_5_num_opt (i:int) p s p1 p2 p3 p4 p5 p0 f = f
  let no_6_num_opt (i:int) p s p1 p2 p3 p4 p5 p6 p0 f = f

  let no_1_all (i:int) s l p g p1 p0 f =  f
  let no_2_all (i:int) s l p g p1 p2 p0 f =  f
  let no_3_all (i:int) s l p g p1 p2 p3 p0 f =  f
  let no_4_all (i:int) s l p g p1 p2 p3 p4 p0 f =  f
  let no_5_all (i:int) s l p g p1 p2 p3 p4 p5 p0 f =  f
  let no_6_all (i:int) s l p g p1 p2 p3 p4 p5 p6 p0 f =  f

(*
type: int ->
  string ->
  ('w23 -> bool) option ->
  ('x23 -> 'y23 -> 'z23 -> 'a24 -> 'b24 -> 'w23) option ->
  bool list ->
  ('x23 -> string) ->
  ('y23 -> string) ->
  ('z23 -> string) ->
  ('a24 -> string) ->
  ('b24 -> string) ->
  ('w23 -> string) ->
  ('x23 -> 'y23 -> 'z23 -> 'a24 -> 'b24 -> 'w23) ->
  'x23 -> 'y23 -> 'z23 -> 'a24 -> 'b24 -> 'w23
*)


end

(* include DebugEmpty *)
include DebugCore

let wrap_z_debug f a b =
  let flag = !z_debug_flag in
  z_debug_flag := true;
  try
    let res = f a b in
    z_debug_flag := flag;
    res
  with _ as e ->
    (z_debug_flag := flag;
     raise e)

let command_args = [
  ("-debug-regexp", Arg.String (fun s ->
       z_debug_file:=("$"^s); z_debug_flag:=true),
   "Match logged methods from a regular expression");
  ("-dre", Arg.String (fun s ->
       (* let _ = print_endline ("!!!-dre "^s) in *)
       z_debug_file:=("$"^s); z_debug_flag:=true;
       read_main ()
     ),
   "Shorthand for -debug-regexp");
  ("-drea", Arg.String (fun s ->
       z_debug_file:=("$.*"); z_debug_flag:=true;
       mk_debug_arg s),
   "Matched input/output with reg-exp");
  ("-dre-trace", Arg.String (fun s ->
       let _ = print_endline ("!!!-dre "^s) in
       z_debug_file:=("$"^s); z_debug_flag:=true;
       debug_pattern_on := true;
       dump_calls:=true;
       dump_calls_all:=true;
       debug_precise_trace:=true;
       debug_pattern := (Str.regexp s)),
   "Matched debug calls and its calees with reg-exp");
  ("-dd", Arg.Unit (fun _ ->
      devel_debug_on :=true;
      devel_debug_steps :=true
     ),
   "Turn on devel_debug on short and normal output");
  ("-debug", Arg.String (fun s ->
       z_debug_file:=s; z_debug_flag:=true),
   "Read from a debug log file");
  ("-dd", Arg.Unit (fun _ ->
      devel_debug_on :=true;
      devel_debug_steps :=true
     ),
   "Turn on devel_debug on short and normal output")
]


