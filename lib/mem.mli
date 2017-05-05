open Numbers

val base_static_pointer : Loc.t

type t

val create : story_file:string -> t

val zversion : t -> Zversion.t

type overwrites [@@deriving sexp]

val get_overwrites : t -> overwrites
val restore_overwrites : t -> overwrites -> t

val size : t -> int

val in_dynamic_memory : t -> Loc.t -> bool

val getb : t -> Loc.t -> Byte.t
val setb : t -> Loc.t -> Byte.t -> t

val getw : t -> Loc.t -> Word.t
val setw : t -> Loc.t -> Word.t -> t

val getloc : t -> Loc.t -> Loc.t
val setloc : t -> Loc.t -> Loc.t -> t

val getc : t -> Loc.t -> char

val get_ascii_string : t -> Loc.t -> len:int -> string
