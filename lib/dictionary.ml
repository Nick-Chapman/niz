open Core.Std
open Numbers

let getb = Mem.getb
let getw = Mem.getw
let dictionary = Header.dictionary

type dictionary = {
  seps : string;
  entry_length : int;
  num_entries : int;
  entries : (int * string) list;
}
[@@deriving sexp_of]

let (++) = Loc.(+)

let get_string mem loc = 
  let module Text = Text.F(struct let the_mem = mem end) in
  fst (Text.decode_string_from loc)

let get_dictionary m loc =
  let n_seps = Byte.to_int (getb m loc) in
  let seps = 
    String.of_char_list (List.map (List.range 1 (n_seps+1)) ~f:(fun i -> 
      Mem.getc m (loc++i)))
  in
  let loc = loc ++ (n_seps + 1) in
  let entry_length = Byte.to_int (getb m loc) in
  let num_entries = Word.to_int (getw m (loc++1)) in
  let entries = 
    List.map (List.range 0 num_entries) ~f:(fun i ->
      i+1, get_string m (loc ++ (3 + i*entry_length)) )
  in
  { seps; entry_length; num_entries; entries }

let all_words m =
  let d = get_dictionary m (dictionary m) in
  List.map d.entries ~f:snd

let dump m = 
  let d = get_dictionary m (dictionary m) in
  printf "%s\n" (Sexp.to_string_hum (sexp_of_dictionary d))


type t = {
  pos : Byte.t;
  length : Byte.t;
  entry: Loc.t; 
}
[@@deriving sexp_of]

let parse m string =
  let lookup =
    let d_loc = dictionary m in
    let dict = get_dictionary m d_loc in
    let offset = d_loc ++ (String.length dict.seps + 4) in
    fun key -> 
      let key6 = String.prefix key 6 in
      match
	List.find dict.entries ~f:(fun (_,entry) ->  key6 = entry)
      with
      | None -> Loc.zero
      | Some (i,_found) -> offset ++ ((i-1) * dict.entry_length)
  in
  let rec loop ~pos acc string =
    match String.lsplit2 ~on:' ' string with
    | Some ("",string) ->
      loop ~pos:(pos+1) acc string
    | Some (left,right) ->
      let length = String.length left in
      let entry = lookup left in
      let t = 
	let length = Byte.of_int_exn length in
	let pos = Byte.of_int_exn pos in
	{ pos; length; entry } 
      in
      loop ~pos:(pos+length+1) (t::acc) right
    | None ->
      let length = String.length string in
      let entry = lookup string in
      let t = 
	let length = Byte.of_int_exn length in
	let pos = Byte.of_int_exn pos in
	{ pos; length; entry } 
      in
      List.rev (t::acc)
  in
  if string = "" then [] else loop ~pos:1 [] string
