open Core
open Numbers

let getb = Mem.getb
let setb = Mem.setb
let getw = Mem.getw
let setw = Mem.setw
let getloc = Mem.getloc
let object_table = Header.object_table

let (++) = Loc.(+)

let get_bytes m loc n = 
  List.map (List.range 0 n) ~f:(fun i -> getb m (loc++i))

let get_words m loc n = 
  List.map (List.range 0 n) ~f:(fun i -> getw m (loc++(2*i)))

type property = {
  number : int;
  size : int;
  data : Byte.t list;
  as_words : int list;
}
[@@deriving sexp_of]

type obj = {
  id : int;
  short_name : string;
  attribute_bits : string;
  parent : int;
  sibling : int;
  child : int;
  properties : property list;
}
[@@deriving sexp_of]

type object_table = {
  loc : Loc.t;
  prop_defaults : int list;
  objects : obj list;
}
[@@deriving sexp_of]

let bitN = Byte.bitN
let set_bitN = Byte.set_bitN
let clear_bitN = Byte.clear_bitN

let showbyte = Byte.to_bitstring

let get_property m loc =
  let byte = getb m loc in
  if Byte.is_zero byte then None else
    let size = (Byte.to_int byte lsr 5) + 1 in
    let number = Byte.to_int byte land 0x1F in
    let data = get_bytes m (loc++1) size in
    
    let rec loop acc = function
      | [] -> assert false
      | [_] ->  List.rev acc
      | x::y::rest ->
	let w = 
	  let x,y = Byte.to_int x, Byte.to_int y in
	  2 * (x * 256 + y) 
	in
	loop (w::acc) (y::rest)
    in
    let as_words = loop [] data in
    assert (List.length as_words + 1 = List.length data);
    Some ({ number; size; data; 
	    as_words
	  }, loc++(size+1))

let get_properties m =
  let rec loop acc loc =
    match get_property m loc with
    | None -> List.rev acc
    | Some (prop,loc) -> loop (prop::acc) loc
  in
  loop []

let max_objects m =
  match Header.release m, Header.serial m with
  | 88,"840726" -> 250
  | _ -> 255

let get_object m ~id loc =
  let attribute_bits = String.concat [
    showbyte (getb m loc);
    showbyte (getb m (loc++1));
    showbyte (getb m (loc++2));
    showbyte (getb m (loc++3));
  ] in
  let parent = Byte.to_int (getb m (loc++4)) in
  let sibling = Byte.to_int (getb m (loc++5)) in
  let child = Byte.to_int (getb m (loc++6)) in
  let prop_table_loc = getloc m (loc++7) in
  let short_name_length = Byte.to_int (getb m prop_table_loc) in
  let start_of_props = prop_table_loc++(short_name_length*2+1) in
  let short_name = 
    if short_name_length = 0 then "" else
      let short_name,_start_of_props' = 
	let module Text = Text.F(struct let the_mem = m end) in
	Text.decode_string_from (prop_table_loc++1) 
      in
      if (start_of_props <> _start_of_props') then (
	failwithf 
	  !"get_object(%d), start_of_props: %{sexp:Loc.t} <> %{sexp:Loc.t}"
	  id start_of_props _start_of_props' ()
      );
      short_name
  in
  let properties = get_properties m start_of_props in
  { id; 
    short_name; 
    attribute_bits; parent; sibling; child; properties; }

let get_object_table m loc = 
  let prop_defaults = List.map (get_words m loc 31) ~f:Word.to_int in
  let objects =
    List.map (List.range 0 (max_objects m)) ~f:(fun i -> 
      get_object m ~id:(i+1) (loc++(62+(i*9))))
  in
  { loc; prop_defaults; objects }

let dump m = 
  let ot = get_object_table m (object_table m) in
  printf "%s\n" (Sexp.to_string_hum (sexp_of_object_table ot))

let get_prop_default m ~p =
  let base = object_table m in
  let loc = base ++ (2*(p-1)) in
  Word.to_int (getw m loc)

let get_attr m ~o ~a =
  let base = object_table m in
  let ah,al = a / 8, 7 - (a mod 8) in
  let loc = base ++ (62 + 9*(o-1) + ah) in
  let res = bitN al (getb m loc) in
  res

let set_attr m ~o ~a =
  let base = object_table m in
  let ah,al = a / 8, 7 - (a mod 8) in
  let loc = base ++ (62 + 9*(o-1) + ah) in
  let old_v = getb m loc in
  let v = set_bitN al old_v in
  setb m loc v

let clear_attr m ~o ~a =
  let base = object_table m in
  let ah,al = a / 8, 7 - (a mod 8) in
  let loc = base ++ (62 + 9*(o-1) + ah) in
  let old_v = getb m loc in
  let v = clear_bitN al old_v in
  setb m loc v

let get_relation offset m ~o =
  let o = Byte.to_int o in
  let base = object_table m in
  let loc = base ++ (62 + 9*(o-1) + offset)  in
  getb m loc

let get_parent  = get_relation 4
let get_sibling = get_relation 5
let get_child  = get_relation 6

let get_short_name m ~o =
  let o = Byte.to_int o in
  let base = object_table m in
  let loc = base ++ (62 + 9*(o-1) + 7) in
  let loc = getloc m loc ++ 1 in
  let module Text = Text.F(struct let the_mem = m end) in
  let name,_ = Text.decode_string_from loc in
  name

let get_prop m ~o ~p =
  let o = Byte.to_int o in
  let base = object_table m in
  let loc = base ++ (62 + 9*(o-1) + 7)  in
  let loc = getloc m loc in
  let loc = loc ++ (1 + 2 * (Byte.to_int (getb m loc))) in
  let rec loop loc =
    match get_property m loc with
    | None -> get_prop_default m ~p
    | Some (prop,loc) -> 
      if prop.number <> p then loop loc else
	match prop.data with
	| [d1] -> Byte.to_int d1
	| [d1;d2] -> 256 * Byte.to_int d1 + Byte.to_int d2
	| data -> 
	  failwithf "get_prop: o=%d, p=%d - size is %d (not 1 or 2)" 
	    o p (List.length data) ()
  in
  loop loc

let get_prop_addr m ~o ~p =
  let o = Byte.to_int o in
  let base = object_table m in
  let loc = base ++ (62 + 9*(o-1) + 7)  in
  let loc = getloc m loc in
  let loc = loc ++ (1 + 2 * Byte.to_int (getb m loc)) in
  let rec loop loc =
    match get_property m loc with
    | None -> Loc.zero (* really, is this in the spec? *)
    | Some (prop,next_loc) -> 
      if prop.number <> p then loop next_loc else
	loc++1
  in
  loop loc

let get_next_prop m ~o ~p =
  let o = Byte.to_int o in
  let base = object_table m in
  let loc = base ++ (62 + 9*(o-1) + 7)  in
  let loc = getloc m loc in
  let loc = loc ++ (1 + 2 * Byte.to_int (getb m loc)) in
  if p = 0 then
    match get_property m loc with
    | None -> 0
    | Some (prop,_) -> prop.number
  else
    let rec loop loc =
      match get_property m loc with
      | None -> failwithf "get_next_prop, no prop %d on object %d" p o ()
      | Some (prop,next_loc) -> 
	if prop.number <> p then loop next_loc else
	  match get_property m next_loc with
	  | None -> 0
	  | Some (prop,_) -> prop.number
    in
    loop loc

let get_prop_len m ~pa =
  let loc = pa ++ (-1) in
    match get_property m loc with
    | None -> failwith "get_prop_len"
    | Some (prop,_) -> prop.size

let put_prop m ~o ~p value =
  let pa = get_prop_addr m ~o ~p in
  let n = get_prop_len m ~pa in
  assert (n >= 0);
  if n > 2 then failwith "put_prop,len>2" else
    if n = 1 then setb m pa (Byte.of_int_exn (value % 256))
    else (assert (n=2); setw m pa (Word.of_int_exn value))

let l_relation offset m o =
  let o = Byte.to_int o in
  let base = object_table m in
  let loc = base ++ (62 + 9*(o-1) + offset)  in
  loc

let l_parent  = l_relation 4
let l_sibling = l_relation 5
let l_child  = l_relation 6

type tree = Tree of Byte.t * tree list

let spaces n = String.init n ~f:(fun _ -> ' ')

let rec print_tree ~print m i (Tree (parent,children)) =
  assert (not (Byte.is_zero parent));
  print (sprintf "%s%s" (spaces i) (get_short_name m ~o:parent));
  List.iter children ~f:(print_tree ~print m (i+4))

let get_children m p =
  let rec loop acc s =
    if Byte.is_zero s then List.rev acc else
      loop (s::acc) (get_sibling m ~o:s)
  in
  loop [] (get_child m ~o:p)

let rec get_object_tree m root = 
  Tree (root,
	List.map (get_children m root) ~f:(get_object_tree m))

let print_object_tree ~print m ~root =
  let tree = get_object_tree m root in
  print "----------";
  print_tree ~print m 0 tree;
  print "----------"

let unlink m this =
  let p = get_parent m ~o:this in
  if Byte.is_zero p then m else
    let c1 = get_child m ~o:p in
    if c1 = this then
      setb m (l_child m p) (get_sibling m ~o:this)
    else
      let rec loop c =
	if Byte.is_zero c then failwith "unlink,loop,0" else
	  let s = get_sibling m ~o:c in
	  if s = this
	  then setb m (l_sibling m c) (get_sibling m ~o:this)
	  else loop s
      in
      loop c1
    
let insert_obj m ~o ~dest =
  let m = unlink m o in
  let m = setb m (l_parent m o) dest in
  let m = setb m (l_sibling m o) (get_child m ~o:dest) in
  let m = setb m (l_child m dest) o in
  m

let remove_obj m ~o =
  let m = unlink m o in
  let m = setb m (l_parent m o) Byte.zero in
  m
