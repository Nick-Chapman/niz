open Core
open Numbers

let map_range (a,b) ~f = List.map (List.range a b) ~f

module F(X : sig val zversion : Zversion.t end) = struct 
  open X
  open Numbers.Zversion

  let (++) = Loc.(+)

  let getb = Mem.getb
  let setb = Mem.setb
  let getw = Mem.getw
  let setw = Mem.setw
  let getloc = Mem.getloc

  (* TODO, intro table_format Small | Big *)

  let num_props =
    match zversion with
    | Z1|Z2|Z3 -> 31
    | Z4 -> 63

  let is_legal_prop p = 
    p >= 1 && p <= num_props

  let num_bytes_for_attributes = 
    match zversion with
    | Z1|Z2|Z3 -> 4
    | Z4 -> 6

  let bits_in_byte = 8
  let num_attributes = bits_in_byte * num_bytes_for_attributes

  let is_legal_attribute a = 
    a >= 0 && a <= num_attributes

  let object_id_size = 
    let open Numbers.Zversion in
    match zversion with
    | Z1|Z2|Z3 -> 1
    | Z4 -> 2

  let relative_count = 3
  let object_entry_size = 
    num_bytes_for_attributes + (relative_count * object_id_size) + 2

  let prop_default_size = 2
  let prop_defaults_table_size = prop_default_size * num_props

  let geto = 
    match zversion with
    | (Z1|Z2|Z3) -> fun m loc -> Obj.of_byte (getb m loc)
    | Z4         -> fun m loc -> Obj.of_word (getw m loc)
      
  let seto = 
    match zversion with
    | (Z1|Z2|Z3) -> fun m loc o -> setb m loc (Obj.to_byte_exn o)
    | Z4         -> fun m loc o -> setw m loc (Obj.to_word o)

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
    let m = setb m (Rel.loc Parent m o) Byte.zero in
    m

  let get_prop_table_loc m o = 
    let loc = 
      object_loc m o
      ++ num_bytes_for_attributes 
      ++ (relative_count * object_id_size)
    in
    getloc m loc

  let get_short_name m o =
    let loc = get_prop_table_loc m o ++ 1 in (* 1 for name-size byte *)
    let module Text = Text.F(struct let the_mem = m end) in
    let name,_ = Text.decode_string_from loc in
    name

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

  type sn = {
    size_size : int; (* always 1 in Z123; 1 or 2 in Z4+ *)
    data_size : int;
    number : int;
  }

  let get_property_size_number_z123 m loc : sn option =
    let byte = getb m loc in
    if Byte.is_zero byte then None else
      let data_size = (Byte.to_int byte lsr 5) + 1 in
      let number = Byte.to_int byte land 0x1F in
      let size_size = 1 in
      Some { size_size; data_size; number }

  let get_property_size_number_z4plus m loc : sn option =
    let byte = getb m loc in
    if Byte.is_zero byte then None else
      let number = Byte.to_int byte land 0x3F in
      let has_second_size_byte = (Byte.bitN 7 byte) in
      let size_size = (if has_second_size_byte then 2 else 1) in
      let data_size =
	if has_second_size_byte
	then 
	  let byte2 = getb m (loc++1) in
	  Byte.to_int byte2 land 0x3F (* TODO: 0 -> 64 *)
	else
	  if Byte.bitN 6 byte then 2 else 1
      in
      Some { size_size; data_size; number }

  let get_property_size_number =
    match zversion with
    | (Z1|Z2|Z3) -> get_property_size_number_z123
    | Z4         -> get_property_size_number_z4plus

  let get_prop_len m ~pa =
    let loc = pa ++ (-1) in (* TODO: Z4 -might be 1 or 2 bytes back *)
    match get_property_size_number m loc with
    | None -> failwith "get_prop_len"
    | Some sn -> sn.data_size


  let get_prop_addr m o ~p =
    let rec loop loc =
      match get_property_size_number m loc with
      | None -> Loc.zero
      | Some sn ->
	let loc = loc ++ sn.size_size in
	if sn.number <> p 
	then loop (loc ++ sn.data_size) 
	else loc
    in
    loop (get_first_prop_loc m o)

  let get_prop m o ~p =
    assert (is_legal_prop p);
    let rec loop loc =
      match get_property_size_number m loc with
      | None -> get_prop_default m ~p
      | Some sn -> 
	let loc = loc ++ sn.size_size in
	if sn.number <> p then loop (loc ++ sn.data_size) else
	  match sn.data_size with
	  | 1 -> Word.of_byte (getb m loc)
	  | 2 -> getw m loc
	  | len -> 
	    failwithf 
	      !"get_prop: o=%{sexp:Obj.t}, p=%d - size is %d (not 1 or 2)" 
	      o p len ()
    in
    loop (get_first_prop_loc m o)

  let get_next_prop m o ~p = (* TODO: Why is this so complicated? *)
    let loc = get_first_prop_loc m o in
    if p = 0 then
      match get_property_size_number m loc with
      | None -> 0
      | Some sn -> sn.number
    else
      let rec loop loc =
	match get_property_size_number m loc with
	| None -> 
	  failwithf 
	    !"get_next_prop, no prop %d on object %{sexp:Obj.t}" p o ()
	| Some sn -> 
	  let next_loc = loc ++ sn.size_size ++ sn.data_size in
	  if sn.number <> p then loop next_loc else
	    match get_property_size_number m next_loc with
	    | None -> 0
	    | Some sn -> sn.number
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

    let max_objects m = 
      (* TODO: How to find this for real? - 
	 objects stop when first prop table begins *)
      match Header.release m, Header.serial m with
      | 88,"840726" -> 250
      | _ -> 255
	  
    type property = {
      number : int;
      data : Byte.t list;
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

    let get_short_name_from_prop_table ~id m ~prop_table_loc = 
      let short_name_length = Byte.to_int (getb m prop_table_loc) in
      let start_of_props = prop_table_loc++(short_name_length*2+1) in
      if short_name_length = 0 then "", start_of_props else
	let short_name,_start_of_props' = 
	  let module Text = Text.F(struct let the_mem = m end) in
	  Text.decode_string_from (prop_table_loc++1) 
	in
	if (start_of_props <> _start_of_props') then (
	  failwithf 
	    !"get_short_name_from_prop_table (%d), start_of_props: %{sexp:Loc.t} <> %{sexp:Loc.t}"
	    id start_of_props _start_of_props' ()
	);
	short_name, start_of_props

    let get_properties m =
      let rec loop acc loc =
	match get_property_size_number m loc with
	| None -> List.rev acc
	| Some {number;size_size;data_size} -> 
	  let loc = loc ++ size_size in
	  let data = map_range (0,data_size) ~f:(fun i -> getb m (loc++i)) in
	  let prop = {number;data} in
	  loop (prop::acc) (loc ++ data_size)
      in
      loop []

    let get_words m loc n = 
      List.map (List.range 0 n) ~f:(fun i -> getw m (loc++(2*i)))

    let showbyte = Byte.to_bitstring

    (* TODO: only used for dumping: recode to avoid *)
    let get_object_id_for_dump =
      match zversion with
      | (Z1|Z2|Z3) -> fun m loc -> Byte.to_int (getb m (loc)), loc++1
      | Z4         -> fun m loc -> Word.to_int (getw m (loc)), loc++2

    let get_object m ~id loc =
      let attribute_bits = 
	String.concat (
	  map_range (0,num_bytes_for_attributes) ~f:(fun i ->
	    showbyte (getb m (loc++i))))
      in
      let loc = loc++num_bytes_for_attributes in
      let parent,loc = get_object_id_for_dump m loc in
      let sibling,loc = get_object_id_for_dump m loc in
      let child,loc = get_object_id_for_dump m loc in
      let prop_table_loc = getloc m loc in
      let short_name,start_of_props = 
	get_short_name_from_prop_table ~id m ~prop_table_loc 
      in
      let properties = get_properties m start_of_props in
      { id; 
	short_name; 
	attribute_bits; parent; sibling; child; properties; }

    let get_object_table m loc = 
      let prop_defaults = 
	List.map (get_words m loc num_props) ~f:Word.to_int in
      let objects =
	List.map (List.range 0 (max_objects m)) ~f:(fun i -> 
	  let offset = prop_defaults_table_size + (i * object_entry_size) in
	  get_object m ~id:(i+1) (loc++offset))
      in
      { loc; prop_defaults; objects }

    let dump m = 
      let ot = get_object_table m (Header.object_table m) in
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
