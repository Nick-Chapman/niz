open Core.Std
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
| Save          of label
| Restore       of label
| Restart
| Ret_popped
| Quit
| New_line
| Show_status
| Verify        of label
| Call          of func * arg list * target
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
| Print_char    of arg
| Print_num     of arg
| Random        of arg * target
| Push          of arg
| Pull          of target
| Input_Stream  of arg
| Output_Stream of arg
[@@deriving sexp_of, variants]

let maybe_branch_loc = 
  function
  | Save(label)
  | Restore(label)
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
    -> let (Branch (_,dest)) = label in
       begin match dest with
       | Dloc (loc) -> Some loc
       | Dfalse | Dtrue -> None
       end
  | Jump loc -> Some loc
  | _ -> None

let is_end = 
  function
  | Rtrue
  | Rfalse
  | Return _
  | Jump _ 
  | Ret_popped
  | Print_ret _
    -> true
  | _ -> false

let sof_i x = Sexp.to_string_hum (sexp_of_t x)
