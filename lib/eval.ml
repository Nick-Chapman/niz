open Core
open Numbers
open Instruction

module F(X : sig 
  val image0 : Mem.t 
  val hide_unimplemented : bool
end) = struct 
  open X

  let is_zork1_release2 = Header.is_zork1_release2 image0

  let zversion = Mem.zversion image0
  let of_packed_address = Loc.of_packed_address zversion

  module Object_table = Object_table.F(struct let zversion = zversion end)

  let get_string mem loc = 
    let module Text = Text.F(struct let the_mem = mem end) in
    fst (Text.decode_string_from loc)

  let get_routine_header mem = 
    let module I_decoder = I_decoder.F(struct let the_mem = mem end) in
    I_decoder.get_routine_header
      
  let read_instruction mem = 
    let module I_decoder = I_decoder.F(struct let the_mem = mem end) in
    I_decoder.get_instruction

  let i_cache = Loc.Table.create()

  module I_decoder0 = I_decoder.F(struct let the_mem = image0 end)
  let read_instruction0 = I_decoder0.get_instruction

  (* With caching, get a factor of 5 speed up! *)
  let read_instruction mem loc =
    if Mem.in_dynamic_memory image0 loc then read_instruction0 loc else
      match Loc.Table.find i_cache loc with
      | Some res -> res
      | None ->
	let res = read_instruction mem loc in
	Loc.Table.add_exn i_cache ~key:loc ~data:res;
	res


  let (++) = Loc.(+)

  type frame = {
    old_stack : Value.t list;
    return_pc : Loc.t;
    target : target;
    n_locals : int;
    locals : Value.t Int.Map.t
  }
  [@@deriving sexp]

  type state = {
    mem : Mem.t;
    base_globals : Value.t;
    pc : Loc.t;
    stack : Value.t list;
    frames : frame list;
  }

  let state0 ~mem = {
    mem;
    base_globals = Value.of_word (Loc.to_word (Header.base_globals mem));
    pc = Header.initial_pc mem;
    stack = [];
    frames = [];
  }

  type status = {
    room : Obj.t * string;
    score : int;
    turns : int;
  }
  [@@deriving sexp]

  type buf_pair = { 
    text_into: Value.t;
    parse_into : Value.t;
  }
  [@@deriving sexp]

  type 'a small_step =
  | Quit
  | Continue of ('a * state)
  | Prompt   of (Mem.t * status * buf_pair * state)

  module ST : sig

    type 'a st
    val quit : unit st
    val prompt : buf_pair -> status -> unit st
    val return : 'a -> 'a st
    val (>>=) : 'a st -> ('a -> 'b st) -> 'b st
    val mkST : (state -> 'a * state) -> 'a st
    val execST : 'a st -> state -> 'a small_step

  end = struct

    type 'a st = ST of (state -> 'a small_step)

    let execST (ST f) = f

    let return x = ST (fun s -> Continue (x,s))

    let mkST f = ST (fun state -> Continue (f state))
      
    let quit = ST (fun _s -> Quit)
      
    let prompt buf_pair status = 
      ST (fun s -> Prompt (s.mem,status,buf_pair,s))

    let (>>=) (ST t) f = 
      ST (fun s -> 
	match t s with
	| Quit -> Quit
	| Continue (a,s) -> execST (f a) s
	| Prompt x -> Prompt x)

  end

  open ST

  let with_state f = mkST (fun s -> f s, s)
  let mod_state f = mkST (fun s -> (), f s)

  let (>>|) st f = st >>= fun x -> return (f x)
  let zip (st1,st2) = st1 >>= fun x1 -> st2 >>= fun x2 -> return (x1,x2)
  let zip3 (st1,st2,st3) = 
    st1 >>= fun x1 -> st2 >>= fun x2 -> st3 >>= fun x3 -> return (x1,x2,x3)

  let memory = with_state (fun s -> s.mem)

  let get_pc = with_state (fun s -> s.pc)
  let set_pc pc = mod_state (fun s -> { s with pc })

  let get_stack = with_state (fun s -> s.stack)
  let set_stack stack = mod_state (fun s -> { s with stack })
  let clear_stack = set_stack []
  let push_stack v = mod_state (fun s -> { s with stack = v :: s.stack })

  let pop_stack = mkST (fun s ->
    match s.stack with
    | v::stack -> v, { s with stack }
    | [] -> 
      if is_zork1_release2
      then Value.of_int 0, s (* workaround for bug in story file*)
      else failwith "pop_stack: stack empty")

  let push_frame frame = 
    mod_state (fun s -> { s with frames = frame :: s.frames })

  let pop_frame = mkST (fun s ->
    match s.frames with
    | [] -> failwith "pop_frame: stack empty"
    | frame::frames -> frame, { s with frames })

  let get_local n = with_state (fun s ->
    match s.frames with
    | [] -> failwith "get_local, no top frame"
    | f::_ ->
      if f.n_locals < n 
      then failwithf "get_local(%d), max=%d" n f.n_locals ()
      else Int.Map.find_exn f.locals n)

  let set_local n v = mod_state (fun s ->
    match s.frames with
    | [] -> failwith "set_local, no top frame"
    | f::fs ->
      if f.n_locals < n 
      then failwithf "set_local(%d), max=%d" n f.n_locals ()
      else
	let locals = Int.Map.add f.locals ~key:n ~data:v in
	let frames = { f with locals } :: fs in
	{ s with frames })

  let store_byte (v1,v2,v3) = 
    mod_state (fun s ->
      let base = Value.to_loc v1 in
      let index = Value.to_int v2 in
      let key = (base ++ index) in
      let data = Value.to_byte v3 in
      { s with mem  = Mem.setb s.mem key data })
      
  let load_byte (v1,v2) =
    with_state (fun s ->
      let base = Value.to_loc v1 in
      let index = Value.to_int v2 in
      let key = (base ++ index) in
      Value.of_byte (Mem.getb s.mem key))

  let store_word (v1,v2,v3) =
    mod_state (fun s ->
      let base = Value.to_loc v1 in
      let index = Value.to_int v2 in
      let key = (base ++ (2*index)) in (* doubling is semantics of opcode *)
      let data = Value.to_loc v3 in
      { s with mem  = Mem.setloc s.mem key data })

  let load_word' (v1,v2) =
    (fun s ->
      let base = Value.to_loc v1 in
      let index = Value.to_int v2 in
      let key = (base ++ (2*index)) in (* doubling is semantics of opcode *)
      Value.of_word (Mem.getw s.mem key))

  let load_word vv = with_state (load_word' vv)

  let base_globals' : (state -> Value.t) = 
    (fun s -> s.base_globals)

  let base_globals = 
    with_state base_globals'

  let get_global' n : (state -> Value.t) = 
    fun s ->
      load_word' (base_globals' s,Value.of_int n) s

  let get_global n = 
    with_state (get_global' n)

  let set_global n v = 
    base_globals >>= fun base_globals ->
    store_word (base_globals,Value.of_int n,v)

  let assign target v = match Target.var target with
    | Sp -> push_stack v
    | Local n -> set_local n v
    | Global n -> set_global n v

  let eval_var target = match Target.var target with
    | Sp -> pop_stack
    | Local n -> get_local n
    | Global n -> get_global n

  let eval = function
    | Con i -> return (Value.of_int i)
    | Var v -> eval_var v

  let eval2 (a,b) = zip (eval a,eval b)
  let eval3 (a,b,c) = zip3 (eval a,eval b,eval c)

  let rec eval_list = function
    | [] -> return []
    | arg::args -> 
      eval arg >>= fun v ->
      eval_list args >>= fun vs ->
      return (v::vs)

  let eval_func = function
    | Floc loc -> return loc 
    | Fvar var -> 
      eval_var var >>= fun v ->
      let loc = of_packed_address (Value.to_word v) in
      return loc

  let setup_locals routine_header actuals =
    let { var_initializations } = routine_header in
    let defaults = List.map var_initializations ~f:(fun i -> Value.of_int i) in
    let n_formals = List.length var_initializations in
    let n_actuals = List.length actuals in
    let values =
      if n_actuals < n_formals
      then actuals @ List.drop defaults n_actuals
      else List.take actuals n_formals
    in
    assert (List.length values = n_formals);
    let alist = List.zip_exn (List.range 1 (n_formals+1)) values in
    Int.Map.of_alist_exn alist, n_formals

  let call func args target =
    eval_func func >>= fun f_loc ->
    if Loc.is_zero f_loc then assign target Value.vfalse else
      eval_list args >>= fun actuals ->
    memory >>= fun mem -> 
    let (routine_header,start) = get_routine_header mem f_loc in
    get_stack >>= fun old_stack ->
    clear_stack >>= fun () ->
    get_pc >>= fun return_pc ->
    let locals,n_locals = setup_locals routine_header actuals in
    let frame = { old_stack; return_pc; target; n_locals; locals } in
    push_frame frame >>= fun () ->
    set_pc start >>= fun () ->
    return ()

  let do_return value =
    pop_frame >>= fun frame ->
    let {old_stack; return_pc; target; n_locals=_; locals=_} = frame in
    set_stack old_stack >>= fun () ->
    assign target value >>= fun () ->
    set_pc return_pc >>= fun () ->
    return ()

  let test_flags (bitmap, flags) =
    let bitmap = Value.to_unsigned bitmap in
    let flags = Value.to_unsigned flags in
    (bitmap land flags) = flags

  let branch label b = 
    let Branch(c,dest) = label in
    if b=c 
    then
      match dest with
      | Dtrue -> do_return Value.vtrue
      | Dfalse -> do_return Value.vfalse
      | Dloc (loc) -> set_pc loc
    else return ()

  let assign_branch_exists target label v =
    assign target v >>= fun () ->
    branch label (Value.is_not_zero v) 

  let show_num v = sprintf "%d" (Value.to_int v)
  let show_char v = sprintf "%c" (Char.of_int_exn (Value.to_int v))

  let show_paddr v =
    memory >>= fun mem -> 
    let loc = of_packed_address (Value.to_word v) in
    return (get_string mem loc)

  let show_addr v =
    memory >>= fun mem -> 
    let loc = Value.to_loc v in
    return (get_string mem loc)

  let store (target,value) = 
    let target = Value.to_byte target in
    if Byte.is_zero target then failwith "store:0" else
      assign (Target.create target) value

  let inc_check(target, v) =
    let target = Target.create (Value.to_byte target) in
    let v = Value.to_int v in
    eval_var target >>= fun tv ->
    let n = Value.to_int tv in
    assign target (Value.of_int (n+1)) >>= fun () ->
    return (n >= v)

  let dec_check(target, v) =
    let target = Target.create (Value.to_byte target) in
    let v = Value.to_int v in
    eval_var target >>= fun tv ->
    let n = Value.to_int tv in
    assign target (Value.of_int (n-1)) >>= fun () ->
    return (n <= v)

  let inc(target) =
    let target = Target.create (Value.to_byte target) in
    eval_var target >>= fun v ->
    assign target (Value.inc v)

  let dec(target) =
    let target = Target.create (Value.to_byte target) in
    eval_var target >>= fun v ->
    assign target (Value.dec v)

  let test_attr (o,a) = 
    let o = Value.to_obj o in
    let a = Value.to_int a in
    with_state (fun s -> 
      Object_table.get_attr s.mem o ~a)

  let show_obj (o) = 
    let o = Value.to_obj o in
    with_state (fun s -> 
      Object_table.get_short_name s.mem o)

  let get_parent (o) = 
    let o = Value.to_obj o in
    with_state (fun s -> 
      Value.of_obj (Object_table.get_parent s.mem o))

  let get_child (o) = 
    let o = Value.to_obj o in
    with_state (fun s -> 
      Value.of_obj (Object_table.get_child s.mem o))

  let get_sibling (o) = 
    let o = Value.to_obj o in
    with_state (fun s -> 
      Value.of_obj (Object_table.get_sibling s.mem o))

  let get_prop (o,p) = 
    let o = Value.to_obj o in
    let p = Value.to_int p in
    with_state (fun s -> 
      Value.of_word (Object_table.get_prop s.mem o ~p))

  let get_prop_addr (o,p) = 
    let o = Value.to_obj o in
    let p = Value.to_int p in
    with_state (fun s -> 
      Value.of_int (Loc.to_int (Object_table.get_prop_addr s.mem o ~p)))

  let get_next_prop (o,p) = 
    let o = Value.to_obj o in
    let p = Value.to_int p in
    with_state (fun s -> 
      Value.of_int (Object_table.get_next_prop s.mem o ~p))

  let get_prop_len pa = 
    let pa = Value.to_loc pa in
    with_state (fun s -> 
      Value.of_int (Object_table.get_prop_len s.mem ~pa))

  let jin (child, parent) =
    let child = Value.to_obj child in
    let parent = Value.to_obj parent in
    with_state (fun s -> 
      Object_table.get_parent s.mem child = parent)

  let insert_obj (o, dest) =
    let o = Value.to_obj o in
    let dest = Value.to_obj dest in
    mod_state (fun s -> 
      { s with mem = Object_table.insert_obj s.mem o ~dest })

  let remove_obj (o) = 
    let o = Value.to_obj o in
    mod_state (fun s -> 
      { s with mem = Object_table.remove_obj s.mem o })

  let put_prop (o,p,v) = 
    let o = Value.to_obj o in
    let p = Value.to_int p in
    let value = Value.to_word v in
    mod_state (fun s ->
      { s with mem = Object_table.put_prop s.mem o ~p value })

  let set_attr (o,a) =
    let o = Value.to_obj o in
    let a = Value.to_int a in
    mod_state (fun s -> 
      { s with mem = Object_table.set_attr s.mem o ~a })

  let clear_attr (o,a) =
    let o = Value.to_obj o in
    let a = Value.to_int a in
    mod_state (fun s -> 
      { s with mem = Object_table.clear_attr s.mem o ~a })

  let write_bytes mem loc xs = 
    fst (List.fold xs ~init:(mem,loc) ~f:(fun (mem,loc) x ->
      Mem.setb mem loc x, loc++1))

  let write_bytes_from_string mem loc string =
    write_bytes mem loc 
      (List.map (String.to_list string) ~f:Byte.of_char)

  let lex_into ~reply buf_pair : (state -> state) = (fun (s:state) ->
    let t_buf_loc = Value.to_loc buf_pair.text_into in
    let p_buf_loc = Value.to_loc buf_pair.parse_into in
    let reply = String.strip (String.lowercase reply) in
    let mem = s.mem in
    let tokens = Dictionary.parse mem reply in
    let bytes_for_parse_buffer =
      Byte.of_int_exn (List.length tokens) ::
	List.concat_map tokens ~f:(fun x ->
	  let high,low = Word.to_high_low (Loc.to_word x.entry) in
	  [high; low; x.length; x.pos])
    in
    (* TODO: should take account of max length for text to input
       & max number of words to parse *)
    let mem = write_bytes_from_string mem (t_buf_loc++1) (reply^"\000") in
    let mem = write_bytes mem (p_buf_loc++1) bytes_for_parse_buffer in
    { s with mem })


  let where_am_i s = 
    Value.to_obj (get_global' 0 s)

  let whats_my_score s = 
    Value.to_int (get_global' 1 s)

  let how_many_turns s = 
    Value.to_int (get_global' 2 s)

  let get_status (s:state) : status = 
    (* This status information is correct only when version <= Z3 *)
    let room = where_am_i s in
    let room_desc = 
      if zversion <= Z3
      then Object_table.get_short_name s.mem room
      else sprintf !"room:%{sexp:Obj.t}" room
    in
    {
      room = (room,room_desc);
      score = whats_my_score s;
      turns = how_many_turns s;
    }

  let sread (text_into,parse_into) =
    with_state get_status >>= fun status ->
    prompt {text_into;parse_into} status 

  type control_state = {
    overwrites : Mem.overwrites;
    pc : Loc.t;
    stack : Value.t list;
    frames : frame list;
  }
  [@@deriving sexp]

  let get_control_state (t:state) = {
    overwrites = Mem.get_overwrites t.mem;
    pc = t.pc;
    stack = t.stack;
    frames = t.frames;
  }

  let restore_control_state (s:state) (cs:control_state) =
    { s with 
      mem = Mem.restore_overwrites s.mem cs.overwrites;
      pc = cs.pc;
      stack = cs.stack;
      frames = cs.frames }

  type save_state =
  (* We dont technically need a status to restore an in-game save,
     but it makes for a more human friendly save-file *)
  | In_game of status * control_state
  (* For an at-prompt save, we do need a status for the [Eval.t]. This
     could be retrieved from the reconstructed state, but again we prefer 
     the more readable save files. *)
  | At_prompt of status * buf_pair * control_state
      [@@deriving sexp]

  let restore_state s ss : state = 
    match ss with
    | In_game (_status,cs) -> restore_control_state s cs
    | At_prompt (_status,buf_pair,cs) ->
      let s = restore_control_state s cs in
      (* Doing an in-game restore with an at-prompt save-state means we
	 have no sensible reply.  *)
      lex_into ~reply:"" buf_pair s

  type callbacks = {
    output : string -> unit;
    trace : Tracing.step -> unit;
    save : save_state -> bool;
    restore : unit -> save_state option;
  }

  let restart = 
    mod_state (fun _s -> state0 ~mem:image0)

  let save cb = 
    with_state (fun s -> 
      cb.save (In_game (get_status s, get_control_state s)))

  let restore cb = 
    mkST (fun s -> 
      match cb.restore() with
      | None -> false, s
      | Some ss -> true, restore_state s ss)

  let scan_table target label (x,table,len) =
    memory >>= fun mem ->
    let x = Value.to_word x in
    let rec loop loc i =
      if i = 0 then Loc.zero,false else
	if Mem.getw mem loc = x then loc,true else
	  loop (loc++2) (i-1)
    in
    let loc,b = loop (Value.to_loc table) (Value.to_int len) in
    let v = Value.of_loc loc in
    assign target v >>= fun () ->
    branch label b

  let execute cb instruction = 
    let game_print string = return (cb.output string) in
    let ignore_printf = 
      if hide_unimplemented
      then fun _s -> return ()
      else fun s -> game_print s
    in
    let ignore0 tag = 
      ignore_printf (sprintf "{%s}\n" tag) in
    let ignore1 tag v = 
      ignore_printf (sprintf !"{%s:%{sexp:Value.t}}\n" tag v) in
    let ignore2 tag v2 = 
      ignore_printf (sprintf !"{%s:%{sexp:Value.t * Value.t}}\n" tag v2) in
    match instruction with
    | Rtrue		      -> do_return Value.vtrue
    | Rfalse                  -> do_return Value.vfalse
    | Print(string)           -> game_print string
    | Print_ret(string)       -> game_print (string^"\n") >>= fun () -> do_return Value.vtrue
    | Save(lab)               -> save cb >>= branch lab
    | Restore(lab)            -> restore cb >>= branch lab
    | Restart                 -> restart
    | Ret_popped              -> pop_stack >>= do_return
    | Quit                    -> quit
    | New_line                -> game_print "\n"
    | Show_status	      -> ignore0 "show-status>"
    | Verify(_lab)            -> return ()
    | Call(f,args,t)          -> call f args t
    | Storew(a,b,c)           -> eval3 (a,b,c) >>= store_word
    | Storeb(a,b,c)           -> eval3 (a,b,c) >>= store_byte
    | Put_prop(a,b,c)         -> eval3 (a,b,c) >>= put_prop
    | Get_sibling(a,t,lab)    -> eval a >>= get_sibling >>= assign_branch_exists t lab
    | Get_child(a,t,lab)      -> eval a >>= get_child >>= assign_branch_exists t lab
    | Get_parent(a,t)         -> eval a >>= get_parent >>= assign t
    | Get_prop_len(a,t)       -> eval a >>= get_prop_len >>= assign t
    | Inc(a)                  -> eval a >>= inc
    | Dec(a)                  -> eval a >>= dec
    | Print_addr(a)           -> eval a >>= show_addr >>= game_print
    | Print_paddr(a)          -> eval a >>= show_paddr >>= game_print
    | Load(arg,t)             -> eval arg >>= assign t
    | Remove_obj(a)           -> eval a >>= remove_obj
    | Print_obj(a)            -> eval a >>= show_obj >>= game_print
    | Return(a)               -> eval a >>= do_return
    | Test_attr(a,b,lab)      -> eval2 (a,b) >>= test_attr >>= branch lab
    | Set_attr(a,b)           -> eval2 (a,b) >>= set_attr
    | Clear_attr(a,b)         -> eval2 (a,b) >>= clear_attr
    | Store (a,b)             -> eval2 (a,b) >>= store
    | Insert_obj(a,b)         -> eval2 (a,b) >>= insert_obj
    | Test(a,b,lab)           -> eval2 (a,b) >>| test_flags >>= branch lab
    | Or_(a,b,t)              -> eval2 (a,b) >>| Value.(lor) >>= assign t
    | And_(a,b,t)             -> eval2 (a,b) >>| Value.(land) >>= assign t
    | Load_word(a,b,t)        -> eval2 (a,b) >>= load_word >>= assign t
    | Load_byte(a,b,t)        -> eval2 (a,b) >>= load_byte >>= assign t
    | Get_prop(a,b,t)         -> eval2 (a,b) >>= get_prop >>= assign t
    | Get_prop_addr(a,b,t)    -> eval2 (a,b) >>= get_prop_addr >>= assign t
    | Get_next_prop(a,b,t)    -> eval2 (a,b) >>= get_next_prop >>= assign t
    | Add(a,b,t)              -> eval2 (a,b) >>| Value.add >>= assign t
    | Sub(a,b,t)              -> eval2 (a,b) >>| Value.sub >>= assign t
    | Mul(a,b,t)              -> eval2 (a,b) >>| Value.mul >>= assign t
    | Div(a,b,t)              -> eval2 (a,b) >>| Value.div >>= assign t
    | Mod_(a,b,t)             -> eval2 (a,b) >>| Value.(mod) >>= assign t
    | Jz(a,lab)               -> eval a >>| Value.is_zero >>= branch lab
    | Dec_check(a,b,lab)      -> eval2 (a,b) >>= dec_check >>= branch lab
    | Inc_check(a,b,lab)      -> eval2 (a,b) >>= inc_check >>= branch lab
    | Je(args,lab)            -> eval_list args >>| Value.equal_any >>= branch lab
    | Jl(a,b,lab)             -> eval2 (a,b) >>| Value.less >>= branch lab
    | Jg(a,b,lab)             -> eval2 (a,b) >>| Value.greater >>= branch lab
    | Jin(a,b,lab)            -> eval2 (a,b) >>= jin >>= branch lab
    | Jump(loc)               -> set_pc loc
    | Sread(a,b)              -> eval2 (a,b) >>= sread
    | Print_char(a)           -> eval a >>| show_char >>= game_print
    | Print_num(a)            -> eval a >>| show_num >>= game_print
    | Random(a,t)             -> eval a >>| Value.random >>= assign t
    | Push(arg)               -> eval arg >>= push_stack
    | Pull(t)                 -> pop_stack >>= assign t
    | Scan_table(a,b,c,t,lab) -> eval3 (a,b,c) >>= scan_table t lab
    | Output_Stream _         -> ignore0 "output_stream"
    | Input_Stream(_arg)      -> ignore0 "input_stream"
    | Erase_window(arg)	      -> eval arg >>= ignore1 "erase_window"
    | Split_window(arg)	      -> eval arg >>= ignore1 "split_window"
    | Set_window(arg)	      -> eval arg >>= ignore1 "set_window"
    | Buffer_mode(arg)	      -> eval arg >>= ignore1 "buffer_mode"
    | Set_cursor(a,b)	      -> eval2 (a,b) >>= ignore2 "set_cursor"
    | Set_text_style(arg)     -> eval arg >>= ignore1 "set_text_style"
    | Read_char(arg)          -> eval arg >>= ignore1 "read_char"
    | Sound_effect(arg)       -> eval arg >>= ignore1 "sound_effect"


  exception Raise_during_execute of 
      Loc.t * Instruction.t * exn * string list
	[@@deriving sexp_of]

  let decode_and_execute ~stepnum (state:state) cb =
    let pc = state.pc in
    let instruction,pc' = read_instruction state.mem pc in
    cb.trace (Tracing.decode ~stepnum pc instruction);
    let state = { state with pc = pc' } in
    try
      execST (execute cb instruction) state
    with exn -> 
      (*let b = String.split_lines (Backtrace.Exn.most_recent()) in*)
      raise (Raise_during_execute (pc,instruction,exn,[]))

  type t = {
    mem : Mem.t;
    ss : save_state;
    status : status;
    k : (reply:string -> callbacks -> t option)
  }

  let run : (callbacks -> state -> t option) =

    let rec on_res ~stepnum cb : (unit small_step -> t option) =
      let stepnum = stepnum + 1 in function
	| Quit -> None
	| Continue ((),s) -> on_state ~stepnum cb s
	| Prompt (mem,status,buf_pair,s) ->
	  let ss = At_prompt (status,buf_pair, get_control_state s) in
	  Some { mem; ss; status; k = (fun ~reply cb ->
	    on_state ~stepnum cb (lex_into ~reply buf_pair s)
	  )}

    and on_state ~stepnum cb state =
      on_res ~stepnum cb 
	(decode_and_execute ~stepnum state cb)
    in

    on_state ~stepnum:0

  let restore_t cb (s:state) ss : t option =
    match ss with
    | In_game (_status,cs) -> 
      run cb (restore_control_state s cs)

    | At_prompt (status,buf_pair,cs) ->
      let s = restore_control_state s cs in
      Some {mem = s.mem; ss; status; k = (fun ~reply cb ->
	run cb (lex_into ~reply buf_pair s)
      )}

  let init ?initial_restore cb =
    let s = state0 ~mem:image0 in
    match initial_restore with 
    | None -> run cb s
    | Some ss -> restore_t cb s ss

  let command t ~reply callbacks = t.k ~reply callbacks

  let room t = snd (t.status.room)

  let score t = t.status.score
  let turns t = t.status.turns

  let save_state t = t.ss

  let display_object_tree t ~print =
    Object_table.print_object_tree ~print t.mem ~root:(fst t.status.room)

end
