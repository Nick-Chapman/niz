
module F(_ : sig
  val image0 : Mem.t 
  val hide_unimplemented : bool
end) : sig

  type t

  type save_state [@@deriving sexp]

  type callbacks = {
    output  : string       -> unit;
    trace   : Tracing.step -> unit;
    save    : save_state   -> bool;
    restore : unit         -> save_state option;
  }

  val init : 
    ?initial_restore:save_state -> 
    callbacks ->
    t option (* None: game executed quit opcode before any user input *)

  val command : 
    t -> 
    reply:string -> 
    callbacks -> 
    t option (* None: game executed quit opcode *)

  val room        : t -> string
  val score       : t -> int
  val turns       : t -> int
  val save_state  : t -> save_state

  val display_object_tree : t -> print:(string -> unit) -> unit

end

