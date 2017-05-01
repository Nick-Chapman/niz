open Numbers

module F(X : sig val the_mem : Mem.t end) : sig

  val get_instruction : Loc.t -> Instruction.t * Loc.t
  val get_routine_header : Loc.t -> Instruction.routine_header * Loc.t
  val disassemble_all : unit -> unit

end
