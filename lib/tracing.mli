open Numbers

type step

val decode : stepnum:int -> Loc.t -> Instruction.t -> step

val program_counter : step -> Loc.t

val display : print:(string -> unit) -> step -> unit

