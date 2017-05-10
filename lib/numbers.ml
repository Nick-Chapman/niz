open Core

module Byte : sig (* 16 bit unsigned : 0..0xFF *)

  type t [@@deriving sexp]
  val of_char : char -> t
  val to_char : t -> char

  val of_int_exn : int -> t
  val to_int : t -> int

  val zero : t
  val is_zero : t -> bool

  val bitN : int -> t -> bool
  val set_bitN : int -> t -> t
  val clear_bitN : int -> t -> t
  val to_bitstring : t -> string

end = struct

  type t = int [@@deriving sexp]
  let in_range i = (i >= 0 && i <= 255)

  let of_char x = int_of_char x
  let to_char t = assert(in_range t); Char.of_int_exn t

  let of_int_exn i = 
    if not (in_range i) then failwith "Byte.of_int_exn"
    else i

  let to_int t = t

  let zero = 0
  let is_zero t = (t = 0)

  let bitN n x = ((x lsr n) land 0x1) = 1
  let set_bitN n x = x lor (0x1 lsl n)
  let clear_bitN n x = x land (lnot (0x1 lsl n))

  let to_bitstring x =
    String.concat (List.rev (List.map (List.range 0 (7+1)) ~f:(fun n -> 
      if bitN n x then "1" else "0")))

end

module Zversion : sig
  type t = Z1 | Z2 | Z3 | Z4 [@@deriving sexp_of]
  val of_byte : Byte.t -> t
end = struct
  type t = Z1 | Z2 | Z3 | Z4 [@@deriving sexp_of]
  let of_byte b =
    match Byte.to_int b with
    | 1 -> Z1
    | 2 -> Z2
    | 3 -> Z3
    | 4 -> Z4
    | n -> failwithf "unsupported z-machine version: %d" n ()
end

module Word : sig (* 16 bit unsigned : 0..0xFFFF *)

  type t [@@deriving sexp] 

  val zero : t
  val of_byte : Byte.t -> t
  val of_high_low : Byte.t * Byte.t -> t
  val of_int_exn : int -> t

  val is_zero : t -> bool

  val to_int : t -> int
  val to_high_low : t -> Byte.t * Byte.t 
  val to_low_byte : t -> Byte.t
  val to_byte_exn : t -> Byte.t

end = struct 

  type t = int [@@deriving sexp]

  let in_range i = (i >= 0 && i <= 0xFFFF)

  let zero = 0

  let of_byte = Byte.to_int
    
  let of_int_exn i = if not (in_range i) then failwith "Byte.of_int_exn" else i

  let of_high_low (high,low) =
    Byte.to_int high * 256 + Byte.to_int low

  let is_zero t = (t=0)

  let to_high_byte t = Byte.of_int_exn (t / 256)
  let to_low_byte  t = Byte.of_int_exn (t % 256)

  let to_high_low t = to_high_byte t, to_low_byte t

  let to_int t = t

  let to_byte_exn = Byte.of_int_exn

end

module Var = struct
  type t = Sp | Local of int | Global of int
  [@@deriving sexp]
end

module Target : sig
  type t [@@deriving sexp]
  val create : Byte.t -> t
  val var : t -> Var.t
end = struct
  type t = Var.t [@@deriving sexp]
  let var x = x
  let create b = match Byte.to_int b with
    | 0 -> Var.Sp
    | n ->
      if n<16 then Var.Local n
      else Var.Global (n-16)
end

module Loc : sig

  type t [@@deriving sexp]
  include Comparable with type t := t
  include Hashable with type t := t

  val of_address : Word.t -> t
  val of_packed_address : Zversion.t -> Word.t -> t
  val of_int : int -> t

  val zero : t
  val is_zero : t -> bool
  val to_word : t -> Word.t
  val to_int : t -> int
  val (+) : t -> int -> t
  val compare : t -> t -> int

  val align_packed_address : Zversion.t -> t -> t

end = struct

  module T = struct
    type t = int [@@deriving sexp,compare]
    let hash x = x
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let create i =
    (*TODO: get version here & keep smaller limit for per Z4 *)
(*    assert (i>=0 && i< 0x20000); (*128k*)*)
    assert (i>=0 && i<= 0x40000); (*256k*)
    i

  let zero = 0

  let is_zero t = (t=0)

  let of_int i = create i

  let of_address w = 
    create (Word.to_int w)

  let packed_address_pointer_size zversion =
    let open Zversion in
    match zversion with
    | Z1|Z2|Z3 -> 2
    | Z4       -> 4

  let of_packed_address zversion = 
    let pointer_size = packed_address_pointer_size zversion in
    fun w -> create (pointer_size * Word.to_int w) 

  let to_word t = Word.of_int_exn t

  let to_int t = t

  let (+) = Int.(+)

  let compare = Int.compare

  let assert_aligned_2 i = assert (i % 2 = 0)
  let align4 i = ((i-1)/4+1)*4

  let align2 i = assert_aligned_2 i; i
  let align4 i = assert_aligned_2 i; align4 i

  let align_packed_address zversion = 
    let open Zversion in
    match zversion with
    | Z1|Z2|Z3 -> align2
    | Z4 -> align4

end

module Obj : sig

  type t [@@deriving sexp]
  val zero : t
  val of_byte : Byte.t -> t
  val of_word : Word.t -> t
  val of_int_exn : int -> t
  val is_zero : t -> bool
  val to_int : t -> int
  val to_byte_exn : t -> Byte.t
  val to_word : t -> Word.t

end = struct

  type t = Word.t [@@deriving sexp]
  let zero = Word.zero
  let of_byte = Word.of_byte
  let of_word w = w
  let of_int_exn = Word.of_int_exn
  let is_zero = Word.is_zero
  let to_int = Word.to_int
  let to_byte_exn = Word.to_byte_exn
  let to_word w = w

end
