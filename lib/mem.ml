open Core
open Numbers

let (++) = Loc.(+)

let base_static_pointer = Loc.of_int 0xE

let header_size = 64

let loc_in_header loc =
  Loc.to_int loc < header_size

let getb_header ~story loc =
  assert (loc_in_header loc);
  Byte.of_char (story.[Loc.to_int loc])

let getw_header ~story loc =
  Word.of_high_low (getb_header ~story loc, getb_header ~story (loc++1))

let getloc_header ~story loc = Loc.of_address (getw_header ~story loc)

let base_static ~story = getloc_header ~story base_static_pointer

type overwrites = Byte.t Loc.Map.t
[@@deriving sexp]

type t = {
  story : string;
  zversion : Zversion.t;
  base_static : Loc.t;
  overwrites : overwrites;
}

let get_overwrites t = t.overwrites
let restore_overwrites t overwrites = { t with overwrites }

let from_string ~story = 
  { story;
    zversion = Zversion.of_byte (getb_header ~story Loc.zero);
    base_static = base_static ~story;
    overwrites = Loc.Map.empty;
  }

let create ~story_file = 
  let story = In_channel.read_all story_file in
  from_string ~story

let zversion t = t.zversion

let size t = String.length t.story

let in_dynamic_memory t loc =
  loc < t.base_static

let getb t loc =
  let i = Loc.to_int loc in
  if (i >= 0 && i < size t) then () else 
    failwithf "getb, i=%d (size=%d)" i (size t) ();
  if not (in_dynamic_memory t loc) then
    Byte.of_char (t.story.[i])
  else
  match Loc.Map.find t.overwrites loc with
  | Some value -> value
  | None -> Byte.of_char (t.story.[i])

let setb t loc byte =
  if not (in_dynamic_memory t loc) then (
    failwithf !"setb: %{sexp:Loc.t}, but dynamic memory stops at: %{sexp:Loc.t}"
      loc t.base_static ()
  );
  let overwrites = Loc.Map.add t.overwrites ~key:loc ~data:byte in
  { t with overwrites }

let getw t i =
  Word.of_high_low (getb t i, getb t (i++1))

let setw t i value =
  let high,low = Word.to_high_low value in
  let t = setb t i high in
  let t = setb t (i++1) low in
  t

let getloc t i = Loc.of_address (getw t i)

let setloc t i loc = setw t i (Loc.to_word loc)

let getc t i = Byte.to_char (getb t i)

let get_ascii_string t i ~len =
  String.of_char_list (List.map (List.range 0 len) ~f:(fun j -> getc t (i++j)))

