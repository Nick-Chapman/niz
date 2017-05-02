open Core
open Numbers

(* TODO: check semantics of all these ops 
   They should all be 16 bit signed ops! 
   And overflow handled by reding modulo 16 bits.
*)

type t = int [@@deriving sexp] 

let make_signed n x =
  if x lsr (n-1) = 1
  then x - (1 lsl n)
  else x

let create x = make_signed 16 x

let of_int = create
let of_byte b = create (Byte.to_int b)

let (lor) (x, y) = create ((lor) x y)
let (land) (x, y) = create ((land) x y)
let add (x, y) = create (x+y)
let sub (x, y) = create (x-y)
let mul (x, y) = create (x*y)
let div (x, y) = create (x/y)
let (mod) (x, y) = create (x mod y)
let inc (x) = create (x+1)
let dec (x) = create (x-1)
let vtrue = create 1 
let vfalse = create 0

let random (n) =
  assert (n>0);
  create (Random.int n + 1)

let is_zero = function 0 -> true | _ -> false
let is_not_zero = function 0 -> false | _ -> true
let greater (x,y) = (x>y)
let less (x,y) = (x<y)

let equal_any = function
  | [] -> failwith "all_equal, []"
  | [_] -> failwith "all_equal, [_] -> false (ever happen?)"
  | [a;b] -> a=b
  | [a;b;c] -> a=b || a=c
  | [a;b;c;d] -> a=b || a=c || a=d
  | _ -> failwith "all_equal, n>4"

let to_unsigned (n) = 
  if n < 0 then 0x10000 + n else n

let to_int (x) = x
let to_loc v = Loc.of_int (to_unsigned v)
let to_word v = Word.of_int_exn (to_unsigned v)
let to_byte v = Byte.of_int_exn (to_unsigned v)
