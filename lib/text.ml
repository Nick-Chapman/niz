open Core
open Core.Poly
open Numbers

module F(X : sig val the_mem : Mem.t end) = struct open X

let (++) = Loc.(+)

let zversion = Header.zversion the_mem

let getw = Mem.getw the_mem

type alphabet = A0 | A1 | A2

module Alphabet = struct

  let alpha0 = "abcdefghijklmnopqrstuvwxyz"
  let alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  let alpha2 =
    if (zversion=Z1)
    then " 0123456789.,!?_#'\"/\\<-:()"
    else " \n0123456789.,!?_#'\"/\\-:()"


  let chars = function
    | A0 -> alpha0
    | A1 -> alpha1
    | A2 -> alpha2

  let shift_up = function
    | A0 -> A1
    | A1 -> A2
    | A2 -> A0

  let shift_down = function
    | A0 -> A2
    | A1 -> A0
    | A2 -> A1

end

let deco alphabet z =
  if not (z>=6 && z<=31) then (
    failwithf "Tex.deco: expect z in range (6,31), got: %d" z ()
  );
  let chars = Alphabet.chars alphabet in
  String.make 1 (chars.[z-6])

let rec read_abbrev n =
  let i =
    let zversion = Zversion.Z3 in (* this is hardcoded here, because this
                     is not technically a packed address
                     but a word address, which always has
                     a wordsize of 2 *)
    Loc.of_packed_address zversion (getw (Header.base_abbrev the_mem ++ (2*n)))
  in
  fst (decode_string_from i)

and decode_string_from =

  let rec inner i ~stop acc ~alpha ~lock zs =
    match zs with
    | [] ->
      if stop
      then String.concat (List.rev acc), i++2
      else loop [] acc ~alpha ~lock (i++2)

    (* 0 - space *)
    | 0::zs ->
      inner i ~stop (" "::acc) ~alpha:lock ~lock zs

    (* 1 - newline in version 1 *)
    | 1::zs when (zversion = Z1) ->
      inner i ~stop ("\n"::acc) ~alpha:lock ~lock zs

    (* 2/3 - shift in version (Z1|Z2) *)
    | 2::zs when (zversion <= Z2 ) ->
      let alpha = Alphabet.shift_up alpha in
      inner i ~stop acc ~alpha ~lock zs

    | 3::zs when (zversion <= Z2) ->
      let alpha = Alphabet.shift_down alpha in
      inner i ~stop acc ~alpha ~lock zs

    (* 1,2,3 - abbreviation  *)
    | ((1|2|3) as z)::znext::zs ->
      let n = (z-1)*32 + znext in
      let expansion = read_abbrev n in
      inner i ~stop (expansion::acc) ~alpha:lock ~lock zs

    (* 1,2,3 - abbreviation; need more chars *)
    | [(1|2|3) as z] -> loop [z] acc ~alpha ~lock (i++2)

    (* 4/5 - shift lock (Z1|Z2) *)
    | 4::zs when (zversion<=Z2) ->
      let alpha = Alphabet.shift_up alpha in
      inner i ~stop acc ~alpha ~lock:alpha zs

    | 5::zs when (zversion<=Z2) ->
      let alpha = Alphabet.shift_down alpha in
      inner i ~stop acc ~alpha ~lock:alpha zs

    (* 4/5 - plain old shift in later versions *)
    | 4::zs ->
      let alpha = Alphabet.shift_up alpha in
      inner i ~stop acc ~alpha ~lock zs

    | 5::zs ->
      let alpha = Alphabet.shift_down alpha in
      inner i ~stop acc ~alpha ~lock zs

    (* 6 (A2) - next two z-chars encoded an ascii char *)
    | 6::a::b::zs when (alpha=A2) ->
      let char =
    let i = (a * 32 + b) in
    assert (i>=0);
    if i>255 (* should never happen in encoded text *)
    then sprintf "<zascii-char-%d>" i
    else sprintf "%c" (Char.of_int_exn i)
      in
      inner i ~stop (char::acc) ~alpha:lock ~lock zs

    (* 6 (A2) - need more chars *)
    | 6::_ as zs when (alpha=A2) ->
      loop zs acc ~alpha ~lock (i++2)

    | z::zs ->
      let char = deco alpha z in
      (* continue using lock alphabet *)
      inner i ~stop (char::acc) ~alpha:lock ~lock zs

  and loop delayed_zs acc ~alpha ~lock i =
    let x = Word.to_int (getw i) in
    let stop = (x lsr 15) land 0x1 = 0x1 in
    let z1 = (x lsr 10) land 0x1f in
    let z2 = (x lsr 5 ) land 0x1f in
    let z3 = (x       ) land 0x1f in

    inner i ~stop acc ~alpha ~lock (delayed_zs @ [z1;z2;z3])

  in
  fun i -> loop [] [] ~alpha:A0 ~lock:A0 i

let align_packed_address = Loc.align_packed_address zversion

let print_between (low,high) =
  let rec loop i =
    let i = align_packed_address i in
    if i >= high then printf !"[%{sexp:Loc.t}]\n" i
    else
      let seg,j = decode_string_from i in
      printf !"[%{sexp:Loc.t}] %s\n" i seg;
      loop j
  in
  loop low

let print_all() =
  let mem = the_mem in
  print_between (Header.text_start mem,
         Header.text_end mem)

end
