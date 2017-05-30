open Numbers

type t [@@deriving sexp]

val of_int	    : int -> t
val of_loc	    : Loc.t -> t
val of_byte         : Byte.t -> t
val of_word         : Word.t -> t
val of_obj	    : Obj.t -> t

val not_	    : t -> t
val (lor)           : t * t -> t
val (land)          : t * t -> t
val add             : t * t -> t
val sub             : t * t -> t
val mul             : t * t -> t
val div             : t * t -> t
val (mod)           : t * t -> t
val inc             : t -> t
val dec             : t -> t
val vtrue           : t
val vfalse          : t
val random          : t -> t (* move to eval *)

val is_zero         : t -> bool
val is_not_zero     : t -> bool
val greater         : t * t -> bool
val less            : t * t -> bool
val equal_any       : t list -> bool

val to_unsigned     : t -> int
val to_int          : t -> int
val to_loc          : t -> Loc.t
val to_word         : t -> Word.t
val to_byte         : t -> Byte.t
val to_obj	    : t -> Obj.t
