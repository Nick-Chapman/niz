open Core
open Numbers

type step =
| Decode of int * Loc.t * Instruction.t
[@@deriving sexp_of]

let decode ~stepnum loc i = Decode (stepnum,loc,i)

let program_counter (Decode (_,pc,_)) = pc

let display ~print step =
  print (String.tr ~target:'\n' ~replacement:' '
	   (Sexp.to_string_hum (sexp_of_step step)))
