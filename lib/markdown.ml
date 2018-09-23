open Parser
open Base
   
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
  | HorizontalRule
  | Table
  | Code of code_type option * string
  | Latex
  
type paragraph =
  | FlatParagraph of { level : int; header : atom list ; content : atom list }
  | Paragraph of { level : int; header : atom list ; content : paragraph list }

let surounded_string l r =
  (* work around *)
  let rec iter a =
    orP (r <<- section []) (lazy (consP any (iter a)))
  in 
  l <<- iter None |> map String.of_char_list
    
let link : atom parser =
  let ref_link =
    map (fun x y -> x, y) (surounded_string (spaces <<- charP '[') (charP ']'))
    |> app (surounded_string (spaces <<- charP '[') (charP ']' ->> spaces))
    |> map (fun (n, u) -> RefLink { name=n ; id=u })
  in
  let url_link =
    map (fun x y -> x, y) (surounded_string (spaces <<- charP '[') (charP ']'))
    |> app (surounded_string (spaces <<- charP '(') (charP ')' ->> spaces))
    |> map (fun (n, u) -> Link { name=n ; url=u })
  in ref_link |-| url_link
   
let tab = stringP "    " <<- spaces   
  
let line : atom list parser = 
  (* Assume that the end of document is a '\n'. *)
  let terminator = (* eof <<- section ' ' |-|  *) charP '\n' <<- section [] in
  let raw_of_any = map (fun c -> Raw (String.of_char c)) any in
  let bullet = spaces <<- (oneOf "*-+" <<- section Uo |-| (sequence [ digits ; stringP "." ] <<- section Ol)) ->> charP ' ' 
             |> map (fun t x -> [List (t,[x])]) in
  let empty_line = spaces <<- charP '\n' in
  let rec code _ = orP (repeat1 empty_line) (lazy (consP any (code None))) in
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
  (* work around *)
  let rec iter : 'a -> atom list parser = fun _ -> 
    orP terminator 
      (lazy (consP (link |-| raw_of_any) (iter None)))
  in
  tab <<- (repeat (code None) |> map List.join |> map String.of_char_list |> map (fun x -> [Code (None,x)])) |-|
  app (iter None) bullet |-|
  iter None |> map collect
  
let lines = line |> repeat |> map List.join

let app_header (p : 'a list parser) : (int * atom list * 'a list) parser =
  let header_line = repeat1 (charP '-') |-| repeat1 (charP '=') <<- section 0 in
  let pre_post_header = repeat1 (charP '#') |> map List.length in
  let header =
    (map (fun x y z -> x, y, z) pre_post_header |> app line)
    ->> tryP 0 (pre_post_header ->> spaces ->> charP '\n') |-|
    (map (fun x y z -> y, x, z) line |> app header_line)
    ->> tryP [] (spaces ->> charP '\n') |-|
    (section (fun z -> 0, [], z))
  in app p header 
  
let paragraph : paragraph parser =
  let flat_paragraph =
    app_header lines
    |> map (fun (x, y, z) -> FlatParagraph { level=x ; header=y ; content=z })
  in
  (* work around *)
  let rec iter a =
    orP flat_paragraph
      (lazy (app_header (repeat (iter a))
             |> (map (fun (x, y, z) -> Paragraph { level=x ; header=y ; content=z }))))
  in iter None 
  
let rec string_of_atom =
  function 
  | Link { name; url } ->
     "Link " ^ name ^ ":" ^ url 
  | RefLink { name; id } ->
     "RefLink " ^ name ^ ":" ^ id 
  | Raw s -> s 
  | List (_,ss) -> 
     List.bind ~f:(fun x -> "List: " :: List.map ~f:string_of_atom x) ss
     |> List.fold_left ~init:"" ~f:(fun a b -> a ^ b)
  | _ -> failwith "error: string_of_atom"
       
let rec string_of_paragraph =
  function 
  | Paragraph { level ; header ; content } ->
     "Paragraph " ^ Int.to_string level ^ ":" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_atom y ^ x) header ^ ":" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_paragraph y ^ x) content ^ "\n"
  | FlatParagraph { level ; header ; content } ->
     "FlatParagraph " ^ Int.to_string level ^ ":" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_atom y ^ x) header ^ ":" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_atom y ^ x) content ^ "\n"
