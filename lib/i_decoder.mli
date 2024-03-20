open Numbers

module F(_ : sig val the_mem : Mem.t end) : sig

  val get_instruction : Loc.t -> Instruction.t * Loc.t
  val get_routine_header : Loc.t -> Instruction.routine_header * Loc.t
  val disassemble_between : Loc.t * Loc.t -> unit
  val disassemble_reachable : unit -> unit
  val disassemble_all : unit -> unit

end
