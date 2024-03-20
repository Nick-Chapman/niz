open Numbers

val zversion        : Mem.t -> Zversion.t
val zversion_string : Mem.t -> string
val release     : Mem.t -> int
val serial      : Mem.t -> string

val is_zork1_release2 : Mem.t -> bool

val base_high       : Mem.t -> Loc.t
val initial_pc      : Mem.t -> Loc.t
val dictionary      : Mem.t -> Loc.t
val object_table    : Mem.t -> Loc.t
val base_globals    : Mem.t -> Loc.t
val base_static     : Mem.t -> Loc.t
val base_abbrev     : Mem.t -> Loc.t

val code_start      : Mem.t -> Loc.t
val code_end        : Mem.t -> Loc.t
val text_start      : Mem.t -> Loc.t
val text_end        : Mem.t -> Loc.t

val print_header    : Mem.t -> unit
