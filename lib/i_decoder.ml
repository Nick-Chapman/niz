open Core
open Numbers
open Instruction
module I = Instruction

let (++) = Loc.(+)

module F(X : sig val the_mem : Mem.t end) = struct open X

let zversion = Mem.zversion the_mem

let of_packed_address = Loc.of_packed_address zversion

module Text = Text.F(struct let the_mem = the_mem end)

let getb = Mem.getb the_mem
let getw = Mem.getw the_mem

let code_start = Header.code_start the_mem
let code_end = Header.code_end the_mem

let get_routine_header loc =
  let nvars = getb loc in
  if Byte.to_int nvars > 15 then (
    failwithf !"too many vars: %{sexp:Byte.t} at: %{sexp:Loc.t}" nvars loc ();
  );
  let loc = loc++1 in
  let (var_initializations,loc) =
    List.fold (List.range 0 (Byte.to_int nvars)) ~init:([],loc) 
      ~f:(fun (acc,loc) _ ->
	(Word.to_int (getw loc) :: acc, loc++2))
  in
  let var_initializations = List.rev var_initializations in
  { var_initializations }, loc

let bitN n x =
  let x = Byte.to_int x in
  ((x lsr n) land 0x1) = 1

type rand_type = ByteConst | WordConst | ByteVariable
[@@deriving sexp_of]

type opcode = int
[@@deriving sexp_of]

type op =
| Op0 of opcode
| Op1 of opcode * rand_type
| Op2 of opcode * rand_type * rand_type
| Op2v of opcode * rand_type list
| OpV of opcode * rand_type list
[@@deriving sexp_of]

let sof_op x = Sexp.to_string_hum (sexp_of_op x)

let read_rand_type x (a,b) =
  match bitN a x, bitN b x with
  | false,false -> Some WordConst
  | false,true -> Some ByteConst
  | true,false -> Some ByteVariable
  | true,true -> None

let read_long_rand_type x a =
  match bitN a x with 
  | true -> ByteVariable
  | false -> ByteConst

let read_var_rand_types x =
  match read_rand_type x (7,6) with
  | None -> []
  | Some ty1 ->
    match read_rand_type x (5,4) with
    | None -> [ty1]
    | Some ty2 ->
      match read_rand_type x (3,2) with
      | None -> [ty1;ty2]
      | Some ty3 ->
	match read_rand_type x (1,0) with
	| None -> [ty1;ty2;ty3]
	| Some ty4 -> [ty1;ty2;ty3;ty4]

type op_type = Long | Short | VarForm

let read_op_type x =
  match bitN 7 x with
  | false -> Long
  | true ->
    match bitN 6 x with
    | false -> Short
    | true -> VarForm

let read_op_and_rand_types loc =
  let x = getb (loc) in
  match read_op_type x with
  | Short ->
    let code = Byte.to_int x land 0xF in
    begin match read_rand_type x (5,4) with
    | None -> Op0 (code), loc++1
    | Some ty -> Op1 (code, ty), loc++1
    end
  | Long ->
    let code = Byte.to_int x land 0x1F in
    let ty1 = read_long_rand_type x 6 in
    let ty2 = read_long_rand_type x 5 in
    Op2 (code, ty1, ty2), loc++1
  | VarForm ->
    let code = Byte.to_int x land 0x1F in
    let y = getb (loc++1) in
    let tys = read_var_rand_types y in
    match bitN 5 x with
    | true -> 
      begin
	match code with
	| 12|26 ->
	  let z = getb (loc++2) in
	  let tys' = read_var_rand_types z in
	  OpV (code, tys @ tys'), loc++3
	| _ -> 
	  OpV (code, tys), loc++2
      end
    | false -> 
      match tys with 
      | [ty1;ty2] -> Op2  (code, ty1, ty2), loc++2
      | _ ->         Op2v (code, tys)     , loc++2

let target loc =
  Target.create (getb loc), loc++1

let make_signed n x =
  if x lsr (n-1) = 1
  then x - (1 lsl n)
  else x

let jump_location loc = 
  let x = Word.to_int (getw loc) in (* should say to_signed_int here *)
  let loc = loc++2 in
  let offset = make_signed 16 x in
  let jloc = loc ++ (offset-2) in
  jloc, loc

let make_dest ~offset loc =
  match offset with
  | 0 -> Dfalse
  | 1 -> Dtrue
  | _ -> Dloc (loc ++ (offset-2))

let label loc = 
  let x = getb loc in
  let sense = bitN 7 x in
  let small = bitN 6 x in
  if small 
  then
    (* interpret 6bit number as unsigned; small braches are always forward! *)
    let offset = (Byte.to_int x land 0x3F) in
    let loc = loc++1 in
    let dest = make_dest ~offset loc in
    Branch (sense,dest), loc
  else
    let y = getb (loc++1) in
    let offset = make_signed 14 ((Byte.to_int x land 0x3F) * 256 
				 + Byte.to_int y) in
    let loc = loc++2 in
    let dest = make_dest ~offset loc in
    Branch (sense,dest), loc

let literal_string loc = 
  Text.decode_string_from loc

let arg rand loc = match rand with
  | ByteConst -> Con (Byte.to_int (getb loc)), loc++1
  | WordConst -> Con (Word.to_int (getw loc)), loc++2
  | ByteVariable -> Var (Target.create (getb loc)), loc++1

let func rand loc = match rand with
  | ByteConst -> failwith "get_func, ByteConst"
  | WordConst -> Floc (of_packed_address (getw loc)), loc++2
  | ByteVariable -> Fvar (Target.create (getb loc)), loc++1

let rec args rands loc = match rands with
  | [] -> [],loc
  | r::rs -> 
    let y,loc = arg r loc in
    let ys,loc = args rs loc in
    y::ys,loc

let take0 instr loc = instr,loc

let take1 instr get1 loc =
  let x1,loc = get1 loc in
  instr x1, loc
    
let take2 instr (get1,get2) loc =
  let arg1,loc = get1 loc in
  let arg2,loc = get2 loc in
  instr arg1 arg2, loc

let take3 instr (get1,get2,get3) loc =
  let arg1,loc = get1 loc in
  let arg2,loc = get2 loc in
  let arg3,loc = get3 loc in
  instr arg1 arg2 arg3, loc

let take5 instr (get1,get2,get3,get4,get5) loc =
  let arg1,loc = get1 loc in
  let arg2,loc = get2 loc in
  let arg3,loc = get3 loc in
  let arg4,loc = get4 loc in
  let arg5,loc = get5 loc in
  instr arg1 arg2 arg3 arg4 arg5, loc

let je2 a b lab = I.Je ([a;b],lab)

let dispatch ~start op = 
  match op with
  | Op0 (0)             -> take0 I.rtrue
  | Op0 (1)             -> take0 I.rfalse
  | Op0 (2)             -> take1 I.print            (literal_string) 
  | Op0 (3)             -> take1 I.print_ret        (literal_string) 
  | Op0 (5)             -> take1 I.save             (label)
  | Op0 (6)             -> take1 I.restore          (label)
  | Op0 (7)             -> take0 I.restart
  | Op0 (8)             -> take0 I.ret_popped
  | Op0 (10)            -> take0 I.quit
  | Op0 (11)            -> take0 I.new_line
  | Op0 (12)            -> take0 I.show_status
  | Op0 (13)            -> take1 I.verify           (label)
  | Op1 (0,x)           -> take2 I.jz               (arg x,label)
  | Op1 (1,x)           -> take3 I.get_sibling      (arg x,target,label)
  | Op1 (2,x)           -> take3 I.get_child        (arg x,target,label)
  | Op1 (3,x)           -> take2 I.get_parent       (arg x,target)
  | Op1 (4,x)           -> take2 I.get_prop_len     (arg x,target)
  | Op1 (5,x)           -> take1 I.inc              (arg x)
  | Op1 (6,x)           -> take1 I.dec              (arg x)
  | Op1 (7,x)           -> take1 I.print_addr       (arg x)
  | Op1 (9,x)           -> take1 I.remove_obj       (arg x)
  | Op1 (10,x)          -> take1 I.print_obj        (arg x)
  | Op1 (11,x)          -> take1 I.return           (arg x)
  | Op1 (12,WordConst)  -> take1 I.jump             (jump_location) 
  | Op1 (13,x)          -> take1 I.print_paddr      (arg x)
  | Op1 (14,x)          -> take2 I.load             (arg x,target)
  | Op2v(1,xs)          -> take2 I.je               (args xs,label)
  | Op2 (1,x,y)         -> take3   je2              (arg x,arg y,label)
  | Op2 (2,x,y)         -> take3 I.jl               (arg x,arg y,label)
  | Op2 (3,x,y)         -> take3 I.jg               (arg x,arg y,label)
  | Op2 (4,x,y)         -> take3 I.dec_check        (arg x,arg y,label)
  | Op2 (5,x,y)         -> take3 I.inc_check        (arg x,arg y,label)
  | Op2 (6,x,y)         -> take3 I.jin              (arg x,arg y,label)
  | Op2 (7,x,y)         -> take3 I.test             (arg x,arg y,label)
  | Op2 (8,x,y)         -> take3 I.or_              (arg x,arg y,target)
  | Op2 (9,x,y)         -> take3 I.and_             (arg x,arg y,target)
  | Op2 (10,x,y)        -> take3 I.test_attr        (arg x,arg y,label)
  | Op2 (11,x,y)        -> take2 I.set_attr         (arg x,arg y)
  | Op2 (12,x,y)        -> take2 I.clear_attr       (arg x,arg y)
  | Op2 (13,x,y)        -> take2 I.store            (arg x,arg y)
  | Op2 (14,x,y)        -> take2 I.insert_obj       (arg x,arg y)
  | Op2 (15,x,y)        -> take3 I.load_word        (arg x,arg y,target)
  | Op2 (16,x,y)        -> take3 I.load_byte        (arg x,arg y,target)
  | Op2 (17,x,y)        -> take3 I.get_prop         (arg x,arg y,target)
  | Op2 (18,x,y)        -> take3 I.get_prop_addr    (arg x,arg y,target)
  | Op2 (19,x,y)        -> take3 I.get_next_prop    (arg x,arg y,target)
  | Op2 (20,x,y)        -> take3 I.add              (arg x,arg y,target)
  | Op2 (21,x,y)        -> take3 I.sub              (arg x,arg y,target)
  | Op2 (22,x,y)        -> take3 I.mul              (arg x,arg y,target)
  | Op2 (23,x,y)        -> take3 I.div              (arg x,arg y,target)
  | Op2 (24,x,y)        -> take3 I.mod_             (arg x,arg y,target)
  | OpV (0,x::xs)       -> take3 I.call             (func x,args xs,target)
  | OpV (1,[x;y;z])     -> take3 I.storew           (arg x,arg y,arg z)
  | OpV (2,[x;y;z])     -> take3 I.storeb           (arg x,arg y,arg z)
  | OpV (3,[x;y;z])     -> take3 I.put_prop         (arg x,arg y,arg z)
  | OpV (4,[x;y])       -> take2 I.sread            (arg x,arg y)
  | OpV (5,[x])         -> take1 I.print_char       (arg x)
  | OpV (6,[x])         -> take1 I.print_num        (arg x)
  | OpV (7,[x])         -> take2 I.random           (arg x,target)
  | OpV (8,[x])         -> take1 I.push             (arg x)
  | OpV (9,[ByteConst]) -> take1 I.pull             (target)
  | OpV (19,[x])        -> take1 I.output_stream1   (arg x)
  | OpV (20,[x])        -> take1 I.input_stream     (arg x)

  (* Trinity - but is valid Z3 *)
  | OpV (10,[x])        -> take1 I.split_window	    (arg x)
  | OpV (11,[x])        -> take1 I.set_window	    (arg x)

  (* Trinity - Z4 *)
  | Op1 (8,x)           -> take2 I.call0	    (func x,target)
  | OpV (13,[x])        -> take1 I.erase_window	    (arg x)
  | OpV (18,[x])        -> take1 I.buffer_mode	    (arg x)
  | OpV (15,[x;y])      -> take2 I.set_cursor	    (arg x, arg y)
  | OpV (17,[x])        -> take1 I.set_text_style   (arg x)
  | Op2 (25,x,y)        -> take3 I.call1	    (func x,arg y,target)
  | OpV (22,[x])        -> take1 I.read_char        (arg x)

  | OpV (23,[x;y;z])    -> take5 I.scan_table (arg x,arg y,arg z,target,label)
  | OpV (19,[x;y])      -> take2 I.output_stream2   (arg x,arg y)
    
  | OpV (12,x::xs)      -> take3 I.call (func x, args xs, target)

  | _ -> 
    fun _loc -> 
      failwithf !"unsupport op at [%{sexp:Loc.t}]: %s" start (sof_op op) ()

let get_instruction loc = 
  let start = loc in
  let op,loc = read_op_and_rand_types loc in
  dispatch ~start op loc

type segment = Segment of (Loc.t * Instruction.t) list * Loc.t
[@@deriving sexp_of]

type routine = Routine of Loc.t * routine_header * segment list
[@@deriving sexp_of]

let print_segment (Segment (located_instructions, end_loc)) =
  List.iter located_instructions ~f:(fun (loc,i) ->
    printf !"[%{sexp:Loc.t}] %{sexp:Instruction.t}\n" loc i
  );
  printf !"[%{sexp:Loc.t}]\n" end_loc

let print_routine (Routine (start,header,segments)) =
  printf "\n--------------------------------------------------";
  printf!"\n[%{sexp:Loc.t}] %s\n" start (sof_routine_header header);
  List.iter segments ~f:print_segment

let read_segment loc = 
  let rec loop acc loc =
    let start = loc in
    let i,loc = get_instruction loc in
    (*printf !"[%{sexp:Loc.t}] %{sexp:Instruction.t}\n" start i;*)
    let acc = (start,i)::acc in
    if is_end i
    then Segment (List.rev acc,loc)
    else loop acc loc
  in 
  loop [] loc

let branch_locations_of_segment (Segment (located_instructions,_)) =
  List.filter_map located_instructions ~f:(fun (_,i) ->
    maybe_branch_loc i)

let read_routine loc =
  printf !"read_routine at [%{sexp:Loc.t}]\n" loc;
  let start = loc in
  let header,loc = get_routine_header loc in
  let rec loop acc in_routine loc = 
    if not (Loc.Set.mem in_routine loc) then List.rev acc else
      let segment = read_segment loc in
      let b_locs = branch_locations_of_segment segment in
      let in_routine = Loc.Set.union in_routine (Loc.Set.of_list b_locs) in
      let Segment(_,loc) = segment in
      loop (segment::acc) in_routine loc
  in
  let segments = loop [] (Loc.Set.singleton loc) loc in
  Routine (start,header,segments)

let call_locs_of_segment (Segment (xs,_)) =
  List.filter_map xs ~f:(fun (_,i) -> maybe_instruction_call_loc i)

let call_locs_of_routine (Routine (_,_,segs)) =
  List.concat_map segs ~f:call_locs_of_segment

let reachable_routines start =
  let rec loop acc done_ todo = 
    match todo with
    | [] -> acc
    | loc::todo ->
      if Loc.Set.mem done_ loc then loop acc done_ todo else
	let done_ = Loc.Set.add done_ loc in
	let routine = read_routine loc in
	loop (routine::acc) done_ (call_locs_of_routine routine @ todo)
  in
  loop [] Loc.Set.empty [start]

let disassemble_reachable () =
  List.iteri
    (List.sort (reachable_routines code_start)
       ~cmp:(fun (Routine(start1,_,_)) (Routine(start2,_,_)) -> 
	 Loc.compare start1 start2))
    ~f:(fun i routine ->
      printf "routine #%d" i;
      print_routine routine)

let align_packed_address = Loc.align_packed_address zversion

let disassemble_all () = 
  let rec loop i loc = 
    let loc = align_packed_address loc in
    if loc >= code_end then () else
      let routine = read_routine loc in
      printf "routine #%d" i;
      print_routine routine;
      let loc = 
	let Routine(_,_,segments) = routine in
	let Segment (_,end_loc) = List.last_exn segments in
	end_loc ++ (Loc.to_int end_loc % 2) (*align even *)
      in 
      loop (i+1) loc
  in
  loop 1 code_start


end
