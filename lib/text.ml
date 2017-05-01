open Core.Std
open Numbers

module F(X : sig val the_mem : Mem.t end) = struct open X
    
let getw = Mem.getw the_mem

let alpha0 = "abcdefghijklmnopqrstuvwxyz"
let alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let alpha2 = "@\n0123456789.,!?_#'\"/\\-:()"

let deco alphabet z =
  assert (String.length alphabet = 26);
  assert (z>=6 && z<32);
  String.make 1 (alphabet.[z-6])

let (++) = Loc.(+)

let rec read_abbrev n =
  let i = Loc.of_packed_address (getw (Header.base_abbrev the_mem ++ (2*n))) in
  fst (decode_string_from i)

and decode_string_from =
  let rec loop delayed_zs acc six alphabet i =
    let x = Word.to_int (getw i) in
    let stop = (x lsr 15) land 0x1 = 0x1 in
    let z1 = (x lsr 10) land 0x1f in
    let z2 = (x lsr 5 ) land 0x1f in
    let z3 = (x       ) land 0x1f in
    let rec inner acc six alphabet = function
      | [] -> 
	if stop 
	then String.concat (List.rev acc), i++2
	else loop [] acc six alphabet (i++2)
      | 0::zs -> inner (" "::acc) false alpha0 zs
      | [1|2|3 as z] -> loop [z] acc six alphabet (i++2)
      | (1|2|3 as z)::znext::zs -> 
	let n = (z-1)*32 + znext in
	let expansion = read_abbrev n in
	inner (expansion::acc) false alpha0 zs
      | 4::zs -> inner acc false alpha1 zs
      | 5::zs -> inner acc true alpha2 zs
      | 6::a::b::zs when six -> 
	let char = 
	  let i = (a * 32 + b) in
	  assert (i>=0);
	  if i>255 (* should never happen in real encoded text *)
	  then sprintf "<zascii-char-%d>" i 
	  else sprintf "%c" (Char.of_int_exn i) 
	in
	inner (char::acc) false alpha0 zs
      | 6::_ as zs when six -> loop zs acc six alphabet (i++2)
      | z::zs -> inner ((deco alphabet z)::acc) false alpha0 zs
    in 
    inner acc six alphabet (delayed_zs @ [z1;z2;z3])
  in 
  fun i -> loop [] [] false alpha0 i

let print_strings_between (low,high) =
  let rec loop i =
    if i >= high then printf !"[%{sexp:Loc.t}]\n" i
    else
      let seg,j = decode_string_from i in
      printf !"[%{sexp:Loc.t}] %s\n" i seg;
      loop j
  in
  loop low

let print_all() = 
  let mem = the_mem in
  print_strings_between (Header.text_start mem, 
			 Loc.of_int (Mem.size mem) ++ -1)

end
