open Core
module TMap = Map.Make(Int)
module RMap = Map.Make(String)
               
type code_type =
  | Shell
[@@deriving sexp]
  
type list_type =
  | Uo
  | Ol
[@@deriving sexp]

type span =
  | AutoLink of string (* TODO *)
  | ImageLink of { name : string ; url : string ; title : string option } (* TODO *)
  | Emphasis of span list
  | StrongEmphasis of span list
  | HorizontalRule (* TODO *)
  | Latex (* TODO *)
  | Ref of string RMap.t
  | Link of { name : string ; url : string }
  | RefLink of { name : string ; id : string }
  | List of list_type * span list list
  | Raw of string
  | Table of string list TMap.t 
  | Code of code_type option * string
[@@deriving sexp]
  
type paragraph_type =
  | Paragraph of { header : (int * span list) option; contents : span list }
  | BlockQuote of paragraph_type
[@@deriving sexp]

let equal_list_type t1 t2 =
  match t1, t2 with
  | Uo, Uo | Ol, Ol -> true
  | _, _ -> false
  
