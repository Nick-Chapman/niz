open Numbers

module F(X : sig val the_mem : Mem.t end) : sig

  val decode_string_from : Loc.t -> string * Loc.t
  val print_all : unit -> unit

end
