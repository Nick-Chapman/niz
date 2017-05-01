open Core.Std
open Numbers
open Mem

let getb' t i = getb t (Loc.of_int i)
let getw' t i = getw t (Loc.of_int i)
let getloc' t i = getloc t (Loc.of_int i)

let zversion t = getb' t 0
let release t = Word.to_int (getw' t 2)
let serial t = get_ascii_string t (Loc.of_int 0x12) ~len:6

let base_high t     = getloc' t 0x4
let initial_pc t    = getloc' t 0x6
let dictionary t    = getloc' t 0x8
let object_table t  = getloc' t 0xA
let base_globals t  = getloc' t 0xC
let base_static t   = getloc t (Mem.base_static_pointer)
let base_abbrev t   = getloc' t 0x18

let (++) = Loc.(+)

let code_start t =
  match serial t with
  | "830330" -> base_high t
  | "840726" -> (initial_pc t) ++ (-1)
  | _ -> (*assert false*) (*base_high t*)
    (initial_pc t) ++ (-1)

let text_start t =
  (* from examination! - how to discover this? *)
  match serial t with
  | "830330" -> Loc.of_int 59094 (* zork1-30 *)
  | "840726" -> Loc.of_int 68374 (* zork1-88 *)
  | "820809" -> Loc.of_int 84812 (* deadline-22 *)
  | "860730" -> Loc.of_int 117644 (* leather goddesses *)
  | _ -> Loc.of_int 100000 (*failwith "text_start, unknown version"*)

let code_end t =
  match serial t with
  | "830330"
  | "840726"
  | "820809"
  | "860730"
    -> text_start t
  | _ ->
    Loc.of_int (size t)

type header = {
  zversion : Byte.t;
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
