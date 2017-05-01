open Numbers

val dump : Mem.t -> unit

val get_prop_default : Mem.t -> p:int -> int

val get_attr : Mem.t -> o:int -> a:int -> bool
val set_attr : Mem.t -> o:int -> a:int -> Mem.t
val clear_attr : Mem.t -> o:int -> a:int -> Mem.t

val get_parent : Mem.t -> o:Byte.t -> Byte.t
val get_sibling : Mem.t -> o:Byte.t -> Byte.t
val get_child : Mem.t -> o:Byte.t -> Byte.t
val get_short_name : Mem.t -> o:Byte.t -> string
val get_prop : Mem.t -> o:Byte.t -> p:int -> int
val get_prop_addr : Mem.t -> o:Byte.t -> p:int -> Loc.t
val get_next_prop : Mem.t -> o:Byte.t -> p:int -> int
val get_prop_len : Mem.t -> pa:Loc.t -> int

val insert_obj : Mem.t -> o:Byte.t -> dest:Byte.t -> Mem.t
val remove_obj : Mem.t -> o:Byte.t -> Mem.t

val put_prop : Mem.t -> o:Byte.t -> p:int -> int -> Mem.t

val print_object_tree : print:(string->unit) -> Mem.t -> root:Byte.t -> unit
