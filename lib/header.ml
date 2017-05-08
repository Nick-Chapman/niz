open Core
open Numbers
open Mem

let getw' t i = getw t (Loc.of_int i)
let getloc' t i = getloc t (Loc.of_int i)

let zversion t = Mem.zversion t
let release t = Word.to_int (getw' t 2)
let serial t = get_ascii_string t (Loc.of_int 0x12) ~len:6

let base_high t     = getloc' t 0x4
let initial_pc t    = getloc' t 0x6
let dictionary t    = getloc' t 0x8
let object_table t  = getloc' t 0xA
let base_globals t  = getloc' t 0xC
let base_static t   = getloc t (Mem.base_static_pointer)
let base_abbrev t   = getloc' t 0x18

(*let zversion t =
  let v = zversion t in
  printf !"INFO: version=%{sexp:Zversion.t}, release=%d, serial=%s\n"
    v (release t) (serial t);
  v*)

let (++) = Loc.(+)

module Known = struct

  (* only used when dumping because I don't have a reliable way of
     knowing which part of the story file is text and which is code *)
  type t = 
  (* .z1 *)
  | Destruct
  | Zork1_2
  (* .z2 *)
  | Zork1_15
  (* .z3 *)
  | Zork1_30
  | Zork1_88
  | Deadline_22
  | Leather_goddesses_59
  (* .z4 *)
  | Trinity_12
      
  exception Unknown
  let story t =
    try 
      let t = match release t, serial t with
	| 01, "030509" -> Destruct
	|  2, "AS000C" -> Zork1_2
	| 15, "UG3AU5" -> Zork1_15
	| 30, "830330" -> Zork1_30
	| 88, "840726" -> Zork1_88
	| 22, "820809" -> Deadline_22
	| 59, "860730" -> Leather_goddesses_59
	| 12, "860926" -> Trinity_12
	| _ -> raise Unknown
      in 
      Some t with | Unknown -> None

end


let is_zork1_release2 t =
  match Known.story t with
  | Some Zork1_2 -> true
  | _ -> false

let code_start t =
  match Known.story t with
  | Some Zork1_30		-> base_high t
  | _				-> (initial_pc t) ++ (-1)

let code_end t =
  (* from examination! - how to discover this? *)
  match Known.story t with
  | Some Zork1_2                -> Loc.of_int 60870
  | Some Zork1_30               -> Loc.of_int 59094
  | Some Zork1_88               -> Loc.of_int 68374
  | Some Deadline_22            -> Loc.of_int 84812
  | Some Leather_goddesses_59   -> Loc.of_int 117644
  | Some Zork1_15               -> Loc.of_int 59686
  | Some Destruct               -> Loc.of_int 0 (*where's the code? *)
  | Some Trinity_12             -> Loc.of_int 251180
  | None                        -> Loc.of_int 0

let text_start t =
  match Known.story t with
  | Some Deadline_22		-> Loc.of_int 100362 (* what's in the gap? *)
  | _				-> code_end t

let text_end t =
  match Known.story t with
  | Some Destruct               -> Loc.of_int 47996
  | _				-> Loc.of_int (Mem.size t) ++ -1


type header = {
  zversion : Zversion.t;
  release : int;
  serial : string;
  size : int;
  code_start : Loc.t;
  text_start : Loc.t;
  initial_pc : Loc.t;
  object_table : Loc.t;
}
[@@deriving sexp_of]

let header t = {
  zversion	    = zversion t;
  release           = release t;
  serial            = serial t;
  size              = size t;
  code_start        = code_start t;
  text_start        = text_start t;
  initial_pc	    = initial_pc t;
  object_table	    = object_table t;
}

let print_header t =
  printf "header: %s\n" (Sexp.to_string_hum (sexp_of_header (header t)))
