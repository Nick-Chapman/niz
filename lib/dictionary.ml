open Core
open Core.Poly
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

let prefix_size zversion =
  let open Numbers.Zversion in
  match zversion with
  | Z1|Z2|Z3 -> 6
  | Z4|Z5    -> 9

let parse m string =
  let zversion = Mem.zversion m in
  let lookup =
    let d_loc = dictionary m in
    let dict = get_dictionary m d_loc in
    let offset = d_loc ++ (String.length dict.seps + 4) in
    fun key ->
      let key6 = String.prefix key (prefix_size zversion) in
      match
    List.find dict.entries ~f:(fun (_,entry) ->  key6 = entry)
      with
      | None -> Loc.zero
      | Some (i,_found) -> offset ++ ((i-1) * dict.entry_length)
  in
  let rec loop ~pos acc = function
    | ""::rest -> loop ~pos:(pos+1) acc rest
    | first::rest ->
      let length = String.length first in
      let entry = lookup first in
      let t =
    let length = Byte.of_int_exn length in
    let pos = Byte.of_int_exn pos in
    { pos; length; entry }
      in
      loop ~pos:(pos+length+1) (t::acc) rest
    | [] -> List.rev acc
  in
  let string = (* TODO: hack for commas; get the pos wrong *)
    (* TODO: seps should come from game file anyway*)
    String.concat_map
      ~f:(function ',' -> " , " | c -> String.make 1 c)
      string
  in
  loop ~pos:1 [] (String.split ~on:' ' string)
