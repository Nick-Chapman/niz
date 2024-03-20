open Numbers

val all_words : Mem.t -> string list

val dump : Mem.t -> unit

type t = {
  pos : Byte.t;
  length : Byte.t;
  entry: Loc.t;
} [@@deriving sexp_of]

val parse : Mem.t -> string -> t list
