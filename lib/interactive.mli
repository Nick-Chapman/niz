
module F(_ : sig

  val story_file : string
  val options : Options.t

end) : sig

  val run : unit -> unit

end
