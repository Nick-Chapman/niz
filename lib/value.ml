open Core
open Numbers

(* TODO: check semantics of all these ops
   They should all be 16 bit signed ops!
   And overflow handled by reding modulo 16 bits.
*)

type t = int [@@deriving sexp]

let to_unsigned (n) =
  if n < 0 then 0x10000 + n else n

let norm = to_unsigned

(*let make_signed n x =
  if x lsr (n-1) = 1
  then x - (1 lsl n)
  else x

let create x = make_signed 16 x*)


let create x =
  let x = x + 65536 in
  let x = x mod 65536 in
  if x>=32768 then x - 65536 else x

let in_range x =
  x >= -32768 && x <= 32767

let create_tag tag x =
  let res = create x in
  if not(in_range res) then
    failwithf "not value.in_range: create(%s) (%d) ->  %d\n" tag x res ();
  res

let create = create_tag "?"

let of_int = create
let of_loc b = create (Loc.to_int b)
let of_byte b = create (Byte.to_int b)
let of_word b = create (Word.to_int b)
let of_obj o  = create (Obj.to_int o)

let not_ x = create (lnot x)
let (lor) (x, y) = create ((lor) x y)
let (land) (x, y) = create ((land) x y)

let add (x, y) = create_tag "+" (norm x+norm y)
let sub (x, y) = create_tag "-" (norm x-norm y)
let mul (x, y) = create_tag "*" (norm x*norm y)

let div (x, y) = create_tag "div" (x/y)
let (mod) (x, y) = create_tag "mod" (x mod y)

let log_shift (x, y) =
  let x = norm x in
  create_tag "log_shift" (
    if y = 0 then x else
      if y > 0 then Int.shift_left x y else
    Int.shift_right x (-y)
  )

let art_shift (x, y) =
  create_tag "art_shift" (
    if y = 0 then x else
      if y > 0 then Int.shift_left x y else
    Int.shift_right x (-y)
  )

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

let to_int (x) = x
let to_loc v = Loc.of_int (to_unsigned v)
let to_word v = Word.of_int_exn (to_unsigned v)
let to_byte v = Byte.of_int_exn (to_unsigned v)
let to_obj v = Obj.of_word (to_word v)
