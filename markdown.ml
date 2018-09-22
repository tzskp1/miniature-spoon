open Parser
open Base
   
type atom =
  | Link of { name : string ; url : string }
  | RefLink of { name : string ; id : string }
  | Raw of string
  | Table
  | Code
  | Latex
  
type paragraph =
  | FlatParagraph of { level : int; header : atom list ; content : atom list }
  | Paragraph of { level : int; header : atom list ; content : paragraph list }

let header_line = repeat (charP '-') |-| repeat (charP '=') <<- section 0

let pre_post_header = repeat (charP '#') |> map List.length 
                
let surounded_string l r =
  (* work around *)
  let rec iter a =
    orP (r <<- section []) (lazy (consP any (iter a)))
  in 
  l <<- iter None |> map String.of_char_list
    
let ref_link =
  sequence [ surounded_string (spaces <<- charP '[') (charP ']') ;
             surounded_string (spaces <<- charP '[') (charP ']' ->> spaces) ]
  |> map (function [n; u] -> RefLink { name=n ; id=u } | _ -> failwith "err:ref_link")
    
let url_link =
  sequence [ surounded_string (spaces <<- charP '[') (charP ']') ;
             surounded_string (spaces <<- charP '(') (charP ')' ->> spaces) ]
  |> map (function [n; u] -> Link { name=n ; url=u } | _ -> failwith "err:url_link")

let link = ref_link |-| url_link
         
let raw_of_any = map (fun c -> Raw (String.of_char c)) any 
               
let rec collect_raws =
  function
  | [] -> []
  | Raw x1 :: Raw x2 :: xs ->
     Raw (x1 ^ x2) :: collect_raws xs
  | x :: xs -> 
     x :: collect_raws xs
    
let end_of_atom = eof <<- section [] |-| sequence [ charP '\n' ; tryP (charP '#') ] <<- section []
                
let end_of_atom' = eof <<- section ' ' |-| charP '\n' <<- section []
                
let rec fix_atom terminator =
  orP terminator 
    (lazy (consP (link |-| raw_of_any) (fix_atom terminator)))

let atoms terminator = fix_atom terminator |> map collect_raws 

let app_header (p : 'a list parser) =
  let header =
    app (map (fun x y z -> x, y, z) pre_post_header) (atoms end_of_atom')
    ->> pre_post_header ->> spaces ->> charP '\n' |-|
      app (map (fun x y z -> x, y, z) header_line) (atoms end_of_atom')
      ->> spaces ->> charP '\n'
  in app header p
  
let paragraph = 
  let flat_paragraph : paragraph parser =
    app_header (atoms end_of_atom)
    |> map (fun (x, y, z) -> FlatParagraph { level=x ; header=y ; content=z })
  in
  (* work around *)
  let rec paragraph a =
    orP flat_paragraph
      (lazy (app_header (repeat (paragraph a)) |> (map (fun (x, y, z) -> Paragraph { level=x ; header=y ; content=z }))))
  in paragraph None 
  
let string_of_atom =
  function 
  | Link { name; url } ->
     "Link " ^ name ^ ":" ^ url ^ "\n"
  | RefLink { name; id } ->
     "RefLink " ^ name ^ ":" ^ id ^ "\n"
  | Raw s -> s ^ "\n"
  | _ -> failwith "error: string_of_atom"
