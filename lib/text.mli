open Numbers

module F(_ : sig val the_mem : Mem.t end) : sig

  val decode_string_from : Loc.t -> string * Loc.t

  val print_between : Loc.t * Loc.t -> unit
  val print_all : unit -> unit

end
