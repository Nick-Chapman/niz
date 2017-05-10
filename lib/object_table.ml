open Core
open Numbers

let map_range (a,b) ~f = List.map (List.range a b) ~f

module F(X : sig val zversion : Zversion.t end) = struct 
  open X

  let (++) = Loc.(+)

  let getb = Mem.getb
  let setb = Mem.setb
  let getw = Mem.getw
  let setw = Mem.setw
  let getloc = Mem.getloc

  type object_table_format = Small | Large

  let format = 
    let open Numbers.Zversion in
    match zversion with
    | Z1|Z2|Z3 -> Small (* max 255 objects *)
    | Z4       -> Large (* max 65535 objects *)

  let num_props =
    match format with
    | Small -> 31
    | Large -> 63

  let is_legal_prop p = 
    p >= 1 && p <= num_props

  let num_bytes_for_attributes = 
    match format with
    | Small -> 4
    | Large -> 6

  let bits_in_byte = 8
  let num_attributes = bits_in_byte * num_bytes_for_attributes

  let is_legal_attribute a = 
    a >= 0 && a <= num_attributes

  let object_id_size = 
    match format with
    | Small -> 1
    | Large -> 2

  let relative_count = 3
  let object_entry_size = 
    num_bytes_for_attributes + (relative_count * object_id_size) + 2

  let prop_default_size = 2
  let prop_defaults_table_size = prop_default_size * num_props

  let geto = 
    match format with
    | Small -> fun m loc -> Obj.of_byte (getb m loc)
    | Large -> fun m loc -> Obj.of_word (getw m loc)
      
  let seto = 
    match format with
    | Small -> fun m loc o -> setb m loc (Obj.to_byte_exn o)
    | Large -> fun m loc o -> setw m loc (Obj.to_word o)

  let object_loc m o =
    let o = Obj.to_int o in
    let base = Header.object_table m in
    base
    ++ prop_defaults_table_size 
    ++ (o-1) * object_entry_size

  let get_object_attr_bit_loc m o ~a =
    assert (is_legal_attribute a);
    let ah,al = a / 8, 7 - (a mod 8) in
    object_loc m o ++ ah, al
      
  let get_attr m o ~a =
    let loc,al = get_object_attr_bit_loc m o ~a in
    Byte.bitN al (getb m loc)

  let set_attr m o ~a =
    let loc,al = get_object_attr_bit_loc m o ~a in
    let old_v = getb m loc in
    let v = Byte.set_bitN al old_v in
    setb m loc v

  let clear_attr m o ~a =
    let loc,al = get_object_attr_bit_loc m o ~a in
    let old_v = getb m loc in
    let v = Byte.clear_bitN al old_v in
    setb m loc v

  module Rel = struct

    type t = Parent | Sibling | Child

    let offset = function
      | Parent -> 0
      | Sibling -> 1
      | Child -> 2

    let loc r m o =
      object_loc m o
      ++ num_bytes_for_attributes
      ++ (offset r * object_id_size)

  end

  let get_parent m o  = geto m (Rel.loc Parent m o)
  let get_sibling m o = geto m (Rel.loc Sibling m o)
  let get_child m o   = geto m (Rel.loc Child m o)

  let unlink m this =
    let p = get_parent m this in
    if Obj.is_zero p then m else
      let c1 = get_child m p in
      if c1 = this then
	seto m (Rel.loc Child m p) (get_sibling m this)
      else
	let rec loop c =
	  if Obj.is_zero c then failwith "unlink,loop,0" else
	    let s = get_sibling m c in
	    if s = this
	    then seto m (Rel.loc Sibling m c) (get_sibling m this)
	    else loop s
	in
	loop c1

  let insert_obj m o ~dest =
    let m = unlink m o in
    let m = seto m (Rel.loc Parent m o) dest in
    let m = seto m (Rel.loc Sibling m o) (get_child m dest) in
    let m = seto m (Rel.loc Child m dest) o in
    m

  let remove_obj m o =
    let m = unlink m o in
    let m = seto m (Rel.loc Parent m o) Obj.zero in
    m

  let get_prop_table_loc m o = 
    let loc = 
      object_loc m o
      ++ num_bytes_for_attributes 
      ++ (relative_count * object_id_size)
    in
    getloc m loc

  let get_short_name m o =
    let loc = get_prop_table_loc m o in
    let module Text = Text.F(struct let the_mem = m end) in
    let short_name_len = Byte.to_int (getb m loc) in
    if short_name_len = 0 then "" else (
      let name,loc_after_name = Text.decode_string_from (loc ++ 1) in
      if not (loc_after_name = loc ++ (1 + 2 * short_name_len)) then (
	failwithf !"failed short_name invariant, o=%{sexp:Obj.t}" o ()
      );
      name)

  let get_prop_default m ~p =
    assert (is_legal_prop p);
    let loc = 
      Header.object_table m
      ++ (p-1) * prop_default_size 
    in
    getw m loc

  let get_first_prop_loc m o = 
    let loc = get_prop_table_loc m o in
    loc ++ (1 + 2 * (Byte.to_int (getb m loc)))

  (* property descriptor *)
  type pdesc = { 
    size_size : int; (* always 1 for Small; 1 or 2 for Large *)
    data_size : int;
    number : int;
  }

  let get_pdesc_small_format m loc : pdesc option =
    let byte = getb m loc in
    if Byte.is_zero byte then None else
      let data_size = (Byte.to_int byte lsr 5) + 1 in
      let number = Byte.to_int byte land 0x1F in
      let size_size = 1 in
      Some { size_size; data_size; number }

  let get_pdesc_large_format m loc : pdesc option =
    let byte = getb m loc in
    if Byte.is_zero byte then None else
      let number = Byte.to_int byte land 0x3F in
      let has_second_size_byte = (Byte.bitN 7 byte) in
      let size_size = (if has_second_size_byte then 2 else 1) in
      let data_size =
	if has_second_size_byte
	then 
	  let byte2 = getb m (loc++1) in
	  let res = Byte.to_int byte2 land 0x3F in
	  let res = (if res = 0 then 64 else res) in
	  res
	else
	  if Byte.bitN 6 byte then 2 else 1
      in
      Some { size_size; data_size; number }

  let get_pdesc =
    match format with
    | Small -> get_pdesc_small_format
    | Large -> get_pdesc_large_format

  let move_back_over_property_size_descriptor =
    match format with
    | Small -> fun _m loc -> loc ++ (-1)
    | Large -> 
      fun m loc ->
	let loc = loc ++ (-1) in
	let byte = getb m loc in
	if Byte.bitN 7 byte (* size_size = 2 *)
	then loc ++ (-1) 
	else loc

  let get_prop_len m ~pa =
    let loc = move_back_over_property_size_descriptor m pa in
    match get_pdesc m loc with
    | None -> failwith "get_prop_len"
    | Some d -> d.data_size

  let get_prop_addr m o ~p =
    let rec loop loc =
      match get_pdesc m loc with
      | None -> Loc.zero
      | Some d ->
	let loc = loc ++ d.size_size in
	if d.number <> p 
	then loop (loc ++ d.data_size) 
	else loc
    in
    loop (get_first_prop_loc m o)

  let get_prop m o ~p =
    let loc = get_prop_addr m o ~p in
    if loc = Loc.zero 
    then get_prop_default m ~p 
    else
      let len = get_prop_len m ~pa:loc in
      match len with
      | 1 -> Word.of_byte (getb m loc)
      | 2 -> getw m loc
      | _ -> 
	failwithf
	  !"get_prop: o=%{sexp:Obj.t}, p=%d - size is %d (not 1 or 2)" 
	  o p len ()

  let get_next_prop m o ~p = 
    let loc = get_first_prop_loc m o in
    if p = 0 then
      (* p=0 return the first prop number of the object *)
      match get_pdesc m loc with
      | None -> 0
      | Some d -> d.number
    else
      let rec loop loc =
	match get_pdesc m loc with
	| None -> 
	  failwithf 
	    !"get_next_prop, no prop %d on object %{sexp:Obj.t}" p o ()
	| Some d -> 
	  let next_loc = loc ++ d.size_size ++ d.data_size in
	  if d.number <> p then loop next_loc else
	    match get_pdesc m next_loc with
	    | None -> 0
	    | Some d -> d.number
      in
      loop loc

  let put_prop m o ~p word =
    let pa = get_prop_addr m o ~p in
    let len = get_prop_len m ~pa in
    match len with
    | 2 -> setw m pa word
    | 1 -> setb m pa (Word.to_low_byte word)
    | _ -> 
      failwithf "put_prop, len=%d (expected 1 or 2)" len ()

  module Dumping = struct
	  
    type property = {
      number : int;
      data : Byte.t list;
    }
    [@@deriving sexp_of]
    
    type obj = {
      id : Obj.t;
      short_name : string;
      attribute_bits : string;
      parent : Obj.t;
      sibling : Obj.t;
      child : Obj.t;
      properties : property list;
    }
    [@@deriving sexp_of]

    type object_table = {
      loc : Loc.t;
      prop_defaults : Word.t list;
      objects : obj list;
    }
    [@@deriving sexp_of]

    let get_properties m o =
      let rec loop acc number =
	if number = 0
	then List.rev acc 
	else
	  let loc = get_prop_addr m o ~p:number in
	  let len = get_prop_len m ~pa:loc in
	  let data = map_range (0,len) ~f:(fun i -> getb m (loc++i)) in
	  let prop = {number;data} in
	  loop (prop::acc) (get_next_prop m o ~p:number)
      in
      loop [] (get_next_prop m o ~p:0)

    let get_object m o =
      let loc = object_loc m o in
      let attribute_bits = 
	String.concat (
	  map_range (0,num_bytes_for_attributes) ~f:(fun i ->
	    Byte.to_bitstring (getb m (loc++i))))
      in
      let parent = get_parent m o in
      let sibling = get_sibling m o in
      let child = get_child m o in
      let short_name = get_short_name m o in
      let properties = get_properties m o in
      { id=o; short_name; attribute_bits; parent; sibling; child; properties; }

    let max_objects m = 
      (* TODO: How to find #objects for real? - 
	 z-standard suggests objects stop when first prop table begins *)
      match Header.release m, Header.serial m with
      | 88,"840726" -> 250
      | _ -> 255

    let get_object_table m = 
      let prop_defaults =  
	map_range (1,1+num_props) ~f:(fun p ->
	  get_prop_default m ~p)
      in
      let objects =
	map_range (1,1+max_objects m) ~f:(fun i -> 
	  let o = Obj.of_int_exn i in
	  get_object m o)
      in 
      {loc = Header.object_table m; prop_defaults; objects }

    let dump m = 
      let ot = get_object_table m in
      printf "%s\n" (Sexp.to_string_hum (sexp_of_object_table ot))

  end

  module ShowTree = struct

    type tree = Tree of Obj.t * tree list

    let spaces n = String.init n ~f:(fun _ -> ' ')

    let rec print_tree ~print m i (Tree (parent,children)) =
      assert (not (Obj.is_zero parent));
      print (sprintf "%s%s" (spaces i) (get_short_name m parent));
      List.iter children ~f:(print_tree ~print m (i+4))

    let get_children m p =
      let rec loop acc s =
	if Obj.is_zero s then List.rev acc else
	  loop (s::acc) (get_sibling m s)
      in
      loop [] (get_child m p)

    let rec get_object_tree m root = 
      Tree (root,
	    List.map (get_children m root) ~f:(get_object_tree m))

    let print_object_tree ~print m ~root =
      let tree = get_object_tree m root in
      print "----------";
      print_tree ~print m 0 tree;
      print "----------"

  end
    
  let dump = Dumping.dump
  let print_object_tree = ShowTree.print_object_tree
    
end
