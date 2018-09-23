open Parser
open Base
   
type atom =
  | Link of { name : string ; url : string }
  | RefLink of { name : string ; id : string }
  | List of atom list list
  | Raw of string
  | Table
  | Code
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
    sequence [ surounded_string (spaces <<- charP '[') (charP ']') ;
               surounded_string (spaces <<- charP '[') (charP ']' ->> spaces) ]
    |> map (function [n; u] -> RefLink { name=n ; id=u } | _ -> failwith "err:ref_link")
  in
  let url_link =
    sequence [ surounded_string (spaces <<- charP '[') (charP ']') ;
               surounded_string (spaces <<- charP '(') (charP ')' ->> spaces) ]
    |> map (function [n; u] -> Link { name=n ; url=u } | _ -> failwith "err:url_link")
  in ref_link |-| url_link
  
let atoms : atom list parser = 
  let terminator = eof <<- section ' ' |-| charP '\n' <<- section [] in
  let raw_of_any = map (fun c -> Raw (String.of_char c)) any in
  let bullet = oneOf "*-+" <<- section [] |-| sequence [ digits ; stringP "." ] in
  let rec collect =
    function
    | [] -> []
    | Raw x1 :: Raw x2 :: xs ->
       Raw (x1 ^ x2) :: xs
       |> collect
    | List x1 :: List x2 :: xs ->
       List (List.append
               (List.map ~f:collect x1)
               (List.map ~f:collect x2)) :: xs
       |> collect
    | x :: xs -> 
       x :: collect xs
  in
  let rec iter : 'a -> atom list parser = fun _ -> 
    orP terminator 
      (lazy (consP (link |-| map (fun x -> List [x]) (spaces <<- bullet <<- (iter None)) |-| raw_of_any) (iter None)))
  in (* work around *)
  iter None |> map collect

let app_header (p : 'a list parser) : (int * atom list * 'a list) parser =
  let header_line = repeat1 (charP '-') |-| repeat1 (charP '=') <<- section 0 in
  let pre_post_header = repeat1 (charP '#') |> map List.length in
  let header =
    (map (fun x y z -> x, y, z) pre_post_header |> app atoms)
    ->> tryP 0 (pre_post_header ->> spaces ->> charP '\n') |-|
    (map (fun x y z -> y, x, z) atoms |> app header_line)
    ->> tryP [] (spaces ->> charP '\n') |-|
    (section (fun z -> 0, [], z))
  in app p header 
  
let paragraph : paragraph parser =
  let flat_paragraph =
    app_header atoms
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
     "Link " ^ name ^ ":" ^ url ^ "\n"
  | RefLink { name; id } ->
     "RefLink " ^ name ^ ":" ^ id ^ "\n"
  | Raw s -> s ^ "\n"
  | List ss -> 
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
