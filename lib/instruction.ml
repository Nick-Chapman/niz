open Core
open Numbers

type routine_header = {
  var_initializations : int list;
}
[@@deriving sexp_of]
let sof_routine_header x = Sexp.to_string_hum (sexp_of_routine_header x)

type dest = Dfalse | Dtrue | Dloc of Loc.t
[@@deriving sexp_of]

type label = Branch of bool * dest
[@@deriving sexp_of]

type arg = 
| Con of int
| Var of Target.t
[@@deriving sexp_of]

type func = 
| Floc of Loc.t
| Fvar of Target.t
[@@deriving sexp_of]

type target = Target.t
[@@deriving sexp]

type t =
| Rtrue
| Rfalse
| Print         of string
| Print_ret     of string
| Save_lab      of label
| Restore_lab   of label
| Restart
| Ret_popped
| Quit
| New_line
| Show_status
| Verify        of label
| Call          of func * arg list * target
| CallN         of func * arg list (* Z5: "N" - discard result *)
| Storew        of arg * arg * arg
| Storeb        of arg * arg * arg
| Put_prop      of arg * arg * arg
| Get_sibling   of arg * target * label
| Get_child     of arg * target * label
| Get_parent    of arg * target
| Get_prop_len  of arg * target
| Inc           of arg
| Dec           of arg
| Print_addr    of arg
| Print_paddr   of arg
| Load          of arg * target
| Remove_obj    of arg
| Print_obj     of arg
| Return        of arg
| Test_attr     of arg * arg * label
| Set_attr      of arg * arg
| Clear_attr    of arg * arg
| Store         of arg * arg
| Insert_obj    of arg * arg
| Test          of arg * arg * label
| Or_           of arg * arg * target
| And_          of arg * arg * target
| Load_word     of arg * arg * target
| Load_byte     of arg * arg * target
| Get_prop      of arg * arg * target
| Get_prop_addr of arg * arg * target
| Get_next_prop of arg * arg * target
| Add           of arg * arg * target
| Sub           of arg * arg * target
| Mul           of arg * arg * target
| Div           of arg * arg * target
| Mod_          of arg * arg * target
| Jz            of arg * label
| Dec_check     of arg * arg * label
| Inc_check     of arg * arg * label
| Je            of arg list * label
| Jl            of arg * arg * label
| Jg            of arg * arg * label
| Jin           of arg * arg * label
| Jump          of Loc.t
| Sread         of arg * arg
| Aread         of arg * arg * target
| Print_char    of arg
| Print_num     of arg
| Random        of arg * target
| Push          of arg
| Pull          of target
| Input_Stream  of arg
| Output_Stream of arg * arg option
(* Trinity - Z3 *)
| Split_window  of arg
| Set_window    of arg
(* Trinity - Z4 *)
| Erase_window  of arg
| Buffer_mode   of arg
| Set_cursor    of arg * arg
| Set_text_style of arg
| Read_char     of arg * target (* arg will always be 1 *)
| Scan_table    of arg * arg * arg * target * label
| Sound_effect  of arg
(* Z4 *)
| Save_tar	of target
| Restore_tar	of target
(* Z5 *)
| Check_arg_count of arg * label
(* judo night *)
| Save_undo	of target
| Restore_undo	of target
| Tokenize	of arg * arg

[@@deriving sexp_of, variants]


let call_vs func args target = call  func args target
let call_vn func args        = calln func args

(* Z4 *)
let call_1s func     target = call_vs func [   ] target
let call_2s func arg target = call_vs func [arg] target

let output_stream1 a = Output_Stream (a,None)
let output_stream2 a b = Output_Stream (a,Some b)

(* Z5 *)
let call_1n func     = call_vn func []
let call_2n func arg = call_vn func [arg]


let maybe_instruction_call_loc = 
  function
  | Call(Floc(loc),_,_) -> Some loc
  | CallN(Floc(loc),_) -> Some loc
  | _ -> None

let maybe_branch_loc = 
  function
  | Save_lab(label)
  | Restore_lab(label)
  | Verify(label)
  | Get_sibling(_,_,label)
  | Get_child(_,_,label)
  | Test_attr (_,_,label)
  | Test (_,_,label)
  | Jz (_,label)
  | Dec_check (_,_,label)
  | Inc_check (_,_,label)
  | Je (_,label) 
  | Jl (_,_,label) 
  | Jg (_,_,label) 
  | Jin (_,_,label) 
  | Scan_table(_,_,_,_,label)
    -> let (Branch (_,dest)) = label in
       begin match dest with
       | Dloc (loc) -> Some loc
       | Dfalse | Dtrue -> None
       end
  | Jump loc -> Some loc
  | _ -> None

let is_end = 
  function
  (*| Quit*) (* having this on breaks disassemble all, coz it's dumb *) (* TODO*)
  | Rtrue
  | Rfalse
  | Return _
  | Jump _ 
  | Ret_popped
  | Print_ret _
    -> true
  | _ -> false

let sof_i x = Sexp.to_string_hum (sexp_of_t x)
