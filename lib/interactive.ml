open Core.Std
open Numbers

let column_width = 80

let two_word_hints = false (* sadly too slow - 10 minutes? *)

let batch_mode = not (Unix.isatty Unix.stdin)

let game_print string = 
  Printf.printf "%s%!" string

let game_print string =

  let print = game_print in
  
  let p_word word col =
    let len = String.length word in
    let n = col + len + 1 in
    if col > column_width
    then (print "\n"; print word; len)
    else (print word; n)
  in    

  let rec p_words col = function 
    | [] -> assert false
    | [word] -> p_word word col
    | word::words -> 
      let col = p_word word col in 
      print " ";
      p_words (col+1) words

  in
  let p_line line = 
    ignore (p_words 0 (String.split ~on:' ' line))
  in

  let rec p_lines = function
    | [] -> assert false
    | [line] -> p_line line
    | line::lines -> p_line line; print "\n"; p_lines lines
  in
  p_lines (String.split ~on:'\n' string)

    

let interpreter_print fmt = 
  ksprintf (fun s -> Printf.printf "%s%!" s) fmt

let save_filename mem =
  (* Can we find the basename of the story somewhere?, i.e. "zork"
     or perhap we can get it from the file in which the mem was loaded *)
  sprintf ".z-save-%d-%s.niz"
    (Header.release mem) (Header.serial mem)

let save_save_state ~filename ss =
  try
    let data = Sexp.to_string_hum (Eval.sexp_of_save_state ss) ^ "\n" in
    interpreter_print "[writing: %s]\n" filename;
    Out_channel.write_all filename ~data;
    true
  with
  | _exn ->
    interpreter_print "[failed to write: %s]\n" filename;
    false
      
let load_save_state ~filename =
  match
    try Some (In_channel.read_all filename) with
    | _exn -> None
  with
  | None -> None (* no file; or failed to read file. Dont write anything as this might be the first time we played this story *)
  | Some s ->
    try
      let sexp = Sexp.of_string (String.strip s) in
      let ss = Eval.save_state_of_sexp sexp in
      interpreter_print "[loaded: %s]\n" filename;
      Some ss
    with
    | _exn -> 
      interpreter_print "[corrupted save file: %s]\n" filename;
      None

let get_reply_from_stdin () = 
  flush(stdout);
  match 
    In_channel.input_line stdin
  with
  | Some s -> Some s
  | None -> 
    printf "\n";
    None

let consider_words (options:Options.t) ~words e =
  let n_words = List.length words in
  if options.trace >= 1 then (
    interpreter_print "cheat: exploring inputs [%d]" n_words
  );
  let current_score = Eval.score e in
  let i_count = ref 0 in
  let get_execution_count() =
    let n = !i_count in
    i_count := 0;
    n
  in
  let callbacks i = 
    let output _ = () in
    let trace = 
      if options.trace >= 1 then (
	fun _ ->
	  incr i_count;
	  if !i_count mod 10000 = 0 then  (
	    if !i_count mod 1000000 = 0 then  (
	      interpreter_print "%s" (sprintf "[%d]" (n_words - i))
	    ) else (
	      interpreter_print "."
	    )
	  );
      ) else
	fun _ -> ()
    in
    let save _ = false in
    let restore () = None in
    { Eval. output; trace; save; restore} 
  in
  let score_changers = 
    List.filter_mapi words ~f:(fun i word ->
      match Eval.command e ~reply:word (callbacks i) with
      | None -> None
      | Some e ->
	match (Eval.score e - current_score) with 
	| 0 -> None
	| delta -> Some (word,delta)
    ) in
  if options.trace >= 1 then (
    let n = get_execution_count() in
    interpreter_print "\n[executed: %d instructions]\n" n;
  );
  List.map score_changers ~f:(fun (w,delta) ->
    sprintf "consider: \"%s\" (%s%d)" w (if delta > 0 then "+" else "") delta)

let split_game_output_string ~output =
  match String.rsplit2 output ~on:'\n' with
  | None -> ("",output)
  | Some (left,right) -> (left^"\n",right)

let print_status_line e = 
  let turns = Eval.turns e in
  let score = Eval.score e in
  let room = Eval.room e in
  interpreter_print "[status: %s, score=%d; turns=%d]\n" room score turns


let run (options:Options.t) ~story_file () = 

  let mem = Mem.create ~file:story_file in

  let () = 
    match Mem.version mem with
    | Z3 -> ()
    | Zunsupported n -> failwithf "unsupport z-version: %d" n ()
  in

  let mem = 
    if options.tandy then (
      let loc = Loc.of_int 0x1 in
      let b = Mem.getb mem loc in
      let b = Byte.set_bitN 3 b in
      Mem.setb mem loc b
    ) 
    else mem
  in

  let save,restore =
    let savefile = save_filename mem in
    save_save_state ~filename:savefile,
    fun () -> load_save_state ~filename:savefile
  in

  let i_count = ref 0 in
  let get_execution_count() =
    let n = !i_count in
    i_count := 0;
    n
  in
  let trace = 
    if options.trace>=9 then 
      fun step -> 
	incr i_count;
	Tracing.display ~print:(interpreter_print "%s\n") step
    else if options.trace>=2 then 
      fun step -> 
	incr i_count;
	interpreter_print !"%{sexp:Numbers.Loc.t} " 
	  (Tracing.program_counter step)
    else if options.trace>=1 then 
      fun _ -> 
	incr i_count
    else 
      fun _ -> ()
  in

  let xs = ref [] in
  let output x = xs := x::(!xs) in
  let gather () = 
    let s = String.concat (List.rev (!xs)) in
    xs := [];
    s
  in

  let callbacks = { Eval. output; trace; save; restore } in

  let dictionary = Dictionary.all_words mem in

  let dictionary = 
    if not two_word_hints then dictionary else
      dictionary @
	List.concat_map dictionary ~f:(fun w1 ->
	  List.map dictionary ~f:(fun w2 -> sprintf "%s %s" w1 w2))
  in

  let objects e = 
    if options.trace >= 9 then 
      Eval.display_object_tree ~print:(interpreter_print "%s\n") e
  in

  let rec loop ~prev_states e =

    let rec get_reply_for_game () =

      objects(e);
      let output = gather() in
      let (output,prompt) = split_game_output_string ~output in
      if options.trace >= 2 then (
	interpreter_print "\n";
      );
      if options.trace >= 1 then (
	let n = get_execution_count() in
	interpreter_print "[executed: %d instructions]\n" n;
      );
      game_print output;
      if not batch_mode then (
	print_status_line e;
	if options.cheat then (
	  List.iter (consider_words options ~words:dictionary e) 
	    ~f:(interpreter_print "%s\n")
	)
      );
      game_print prompt;
      if prompt = "" then game_print ">>"; (*hmm..hack for reload*)
      
      match get_reply_from_stdin() with
      | None -> 
	if not batch_mode then (
	  ignore (save (Eval.save_state e))
	)

      | Some reply ->
	
	if batch_mode then (
	  interpreter_print "%s\n" reply
	);
	match reply with
	  
	| "#tree" ->
	  Eval.display_object_tree ~print:(interpreter_print "%s\n") e;
	  get_reply_for_game()
	    
	| "#undo" ->
	  begin match prev_states with
	  | [] ->
	    interpreter_print "[nothing to undo]\n"; 
	    get_reply_for_game()
	  | e::prev_states -> loop ~prev_states e
	  end

	| _ -> 
	  let prev_states = e :: prev_states in
	  match Eval.command e ~reply callbacks with
	  | None -> ()
	  | Some  e -> loop ~prev_states e
    in
    get_reply_for_game()
  in

  match
    let opt_ss = restore() in
    Eval.init mem 
      ?initial_restore:opt_ss 
      callbacks
  with
  | None -> ()
  | Some e -> loop ~prev_states:[] e
