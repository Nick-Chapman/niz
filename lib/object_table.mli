open Numbers

module F(_ : sig val zversion : Zversion.t end) : sig

  val get_attr   : Mem.t -> Obj.t -> a:int -> bool
  val set_attr   : Mem.t -> Obj.t -> a:int -> Mem.t
  val clear_attr : Mem.t -> Obj.t -> a:int -> Mem.t

  val get_parent  : Mem.t -> Obj.t -> Obj.t
  val get_sibling : Mem.t -> Obj.t -> Obj.t
  val get_child   : Mem.t -> Obj.t -> Obj.t
  val insert_obj  : Mem.t -> Obj.t -> dest:Obj.t -> Mem.t
  val remove_obj  : Mem.t -> Obj.t -> Mem.t

  val get_short_name : Mem.t -> Obj.t -> string
  val get_prop       : Mem.t -> Obj.t -> p:int -> Word.t
  val put_prop       : Mem.t -> Obj.t -> p:int -> Word.t -> Mem.t
  val get_next_prop  : Mem.t -> Obj.t -> p:int -> int
  val get_prop_addr  : Mem.t -> Obj.t -> p:int -> Loc.t
  val get_prop_len   : Mem.t -> pa:Loc.t -> int

  val dump : Mem.t -> unit
  val print_object_tree : print:(string->unit) -> Mem.t -> root:Obj.t -> unit

end
