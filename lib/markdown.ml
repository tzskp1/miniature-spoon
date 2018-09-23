open Parser
open Core
module TMap = Map.Make(Int)
   
type code_type =
  | Shell
  
type list_type =
  | Uo
  | Ol
  
type atom =
  | Link of { name : string ; url : string }
  | RefLink of { name : string ; id : string }
  | List of list_type * atom list list
  | Raw of string
  | Table of string list TMap.t 
  | Code of code_type option * string
  | HorizontalRule
  | Latex
  
type paragraph =
  | Paragraph of { level : int; header : atom list ; content : paragraph list }
  | PrimParagraph of atom list

let surounded_string l r =
  (* work around *)
  let rec iter _ =
    orP (r <<- section []) (lazy (consP any (iter None)))
  in 
  l <<- iter None |> map ~f:String.of_char_list
    
let link : atom parser =
  let ref_link =
    app (surounded_string (spaces <<- charP '[') (charP ']' ->> spaces))
      ~f:(map ~f:(fun x y -> x, y) (surounded_string (spaces <<- charP '[') (charP ']')))
    |> map ~f:(fun (n, u) -> RefLink { name=n ; id=u })
  in
  let url_link =
    app (surounded_string (spaces <<- charP '(') (charP ')' ->> spaces))
      ~f:(map ~f:(fun x y -> x, y) (surounded_string (spaces <<- charP '[') (charP ']')))
    |> map ~f:(fun (n, u) -> Link { name=n ; url=u })
  in ref_link |-| url_link
   
let line eof : atom list parser = 
  (* Assume that the end of document is a '\n'. *)
  let terminator = eof <<- section [] in
  let tab = stringP "    " <<- spaces in
  let raw_of_any = map ~f:(fun c -> Raw (String.of_char c)) any in
  let bullet = spaces <<- (oneOf "*-+" <<- section Uo |-| (sequence [ digits ; stringP "." ] <<- section Ol)) ->> charP ' ' 
             |> map ~f:(fun t x -> [List (t,[x])]) in
  let code =
    let empty_line = spaces <<- charP '\n' in
    let rec iter _ = 
      orP (repeat1 empty_line) (lazy (consP any (iter None)))
    in tab <<- (repeat (iter None) |> map ~f:List.join |> map ~f:String.of_char_list |> map ~f:(fun x -> [Code (None,x)])) in
  
  let table =
    let t_col = spaces <<- charP '|' ->> spaces in
    let table_line_col =
      t_col <<- map ~f:String.of_char_list (until (spaces <<- charP '|' |-| charP '\n')) in
    let table_line = repeat table_line_col ->> spaces ->> charP '\n' in
    let check_table = repeat1 (t_col <<- repeat1 (charP '-')) ->> t_col ->> charP '\n' in
    let zip_with_num xs =
      let num = List.length xs in List.zip_exn xs (List.range 0 num)
    in
    let label = map table_line ~f:(fun x -> TMap.add_exn TMap.empty ~key:0 ~data:x) ->> check_table
    in app (repeat table_line)
         ~f:(map label
               ~f:(fun x xs -> List.fold_left (zip_with_num xs) ~init:x
                                 ~f:(fun b (a,n) -> TMap.add_exn b ~key:(n + 1) ~data:a)))
       |> map ~f:(fun x -> [Table x])
  in
  
  let rec collect =
    function
    | [] -> []
    | Raw x1 :: Raw x2 :: xs ->
       Raw (x1 ^ x2) :: xs
       |> collect
    | List (t1,x1) :: List (t2,x2) :: xs when phys_equal t1 t2 ->
       List (t1,(List.append
                   (List.map ~f:collect x1)
                   (List.map ~f:collect x2))) :: xs
       |> collect
    | List (t,x) :: xs ->
       List (t,(List.map ~f:collect x)) :: collect xs
    | x :: xs -> 
       x :: collect xs
  in
  (* character wise operation which associate with list/link parsing *)
  let rec iter _ : atom list parser =
  (* work around *)
    orP terminator 
      (lazy (consP (link |-| raw_of_any) (iter None)))
  in
  table |-| code |-| app (iter None) ~f:bullet |-| iter None |> map ~f:collect
          
let paragraph : paragraph parser =
  let line_n = line (repeat (charP '#') <<- spaces <<- (charP '\n') ->> spaces) in
  let header_line = repeat (charP '-') |-| repeat (charP '=') <<- section 0 in
  let pre_header = (spaces <<- repeat (charP '#') ->> spaces) |> map ~f:List.length in
  let pre = map ~f:(fun x y z -> x, y, z) pre_header in
  let title = map ~f:(fun x y z -> y, x, z) (line (charP '\n')) in
  let header =
    app line_n ~f:pre |-|
    (app header_line ~f:title) ->> tryP (spaces <<- charP '\n') in
  let s_line_n = failP (spaces <<- (charP '#')) <<- line (charP '\n') in
  let prim = repeat1 s_line_n |> map ~f:List.join |> map ~f:(fun x -> PrimParagraph x) in
  let rec iter _ =
    orP prim
      (lazy (app (repeat (iter None)) ~f:header
       |> map ~f:(fun (x, y, z) -> Paragraph { level=x ; header=y ; content=z })))
  in iter None
  
let rec string_of_atom =
  function 
  | Link { name; url } ->
     "Link " ^ name ^ ":" ^ url 
  | RefLink { name; id } ->
     "RefLink " ^ name ^ ":" ^ id 
  | Raw s -> s 
  | List (_, ss) -> 
     List.bind ~f:(fun x -> "List:" :: List.map ~f:string_of_atom x) ss
     |> List.fold_left ~init:"" ~f:(^)
  | Code (_, s) -> "Code:" ^ s
  | Table tbl ->
     "Table:" ^ 
     List.fold_left (TMap.to_alist tbl) ~init:"" ~f:(fun l (k,d) -> l ^ (Int.to_string k) ^ (List.fold_left ~init:"" ~f:(^) d) ^ "\n")
  | _ -> failwith "error: string_of_atom"
       
let rec string_of_paragraph =
  function 
  | Paragraph { level ; header ; content } ->
     "Paragraph " ^ Int.to_string level ^ ":" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_atom y ^ x) header ^ ":" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_paragraph y ^ x) content ^ "\n"
  | PrimParagraph content ->
     "PrimParagraph" ^ ":" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_atom y ^ x) content ^ "\n"
