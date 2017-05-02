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

  let of_int_exn i = if not (in_range i) then failwith "Byte.of_int_ex" else i
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
  type t = Z3 | Zunsupported of int
  val of_byte : Byte.t -> t
end = struct
  type t = Z3 | Zunsupported of int
  let of_byte b =
    match Byte.to_int b with
    | 3 -> Z3
    | n -> Zunsupported n
end

module Word : sig (* 16 bit unsigned : 0..0xFFFF *)

  type t [@@deriving sexp_of] 

  val of_int_exn : int -> t
  val to_int : t -> int

  val of_high_low : Byte.t * Byte.t -> t
  val to_high_low : t -> Byte.t * Byte.t 

end = struct 

  type t = int [@@deriving sexp_of]

  (* was BUG - Had "||" instead of "&&" at one point & failed to detect -1 *)
  let in_range i = (i >= 0 && i <= 0xFFFF)

  let of_int_exn i = if not (in_range i) then failwith "Byte.of_int_exn" else i
  let to_int t = t

  let of_high_low (high,low) =
    Byte.to_int high * 256 + Byte.to_int low

  let to_high_low t = 
    Byte.of_int_exn (t / 256), Byte.of_int_exn (t % 256)

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

  val of_address : Word.t -> t
  val of_packed_address : Word.t -> t
  val of_int : int -> t

  val zero : t
  val is_zero : t -> bool
  val to_word : t -> Word.t
  val to_int : t -> int
  val (+) : t -> int -> t

end = struct

  module T = struct
    type t = int [@@deriving sexp,compare]
  end
  include T
  include Comparable.Make(T)

  let create i =
    assert (i>=0 && i<= 0x1FFFF); (*128k*)
    i

  let zero = 0

  let is_zero t = (t=0)

  let of_int i = create i

  let of_address w = 
    create (Word.to_int w)

  let of_packed_address w = 
    create (2 * Word.to_int w) (* doubling! 64k -> 128k *)

  let to_word t = Word.of_int_exn t

  let to_int t = t

  let (+) = Int.(+)

end
