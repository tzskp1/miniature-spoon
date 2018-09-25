open Parser
open Core
module TMap = Map.Make(Int)
module RMap = Map.Make(String)
   
type code_type =
  | Shell
  
let equal_code_type t1 t2 =
  match t1,t2 with
  | Shell, Shell -> true
  
type list_type =
  | Uo
  | Ol

let equal_list_type t1 t2 =
  match t1,t2 with
  | Uo, Uo | Ol, Ol -> true
  | _, _ -> false
  
type atom =
  | AutoLink of string (* TODO *)
  | ImageLink of { name : string ; url : string ; title : string option } (* TODO *)
  | Emphasis of atom list (* TODO *)
  | HorizontalRule (* TODO *)
  | Latex (* TODO *)
  | Ref of string RMap.t
  | Link of { name : string ; url : string }
  | RefLink of { name : string ; id : string }
  | List of list_type * atom list list
  | Raw of string
  | Table of string list TMap.t 
  | Code of code_type option * string
  
type paragraph_type =
  | Paragraph of { level : int; header : atom list ; content : paragraph_type list }
  | PrimParagraph of atom list
  | BlockQuote of paragraph_type (* TODO *)
  
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
     "PrimParagraph:" ^
       List.fold_left ~init:"" ~f:(fun x y -> string_of_atom y ^ x) content ^ "\n"
  | BlockQuote paragraph ->
     "BlockQuote:" ^ string_of_paragraph paragraph
    
let fold_lump piece =
  List.fold_left ~init:"" ~f:(fun b a -> b ^ (piece a))

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
   
   
let empty_line = spaces <<- charP '\n'
               
let line eof : atom list parser = 
  (* Assume that the end of document is a '\n'. *)
  let terminator = eof <<- section [] in
  let tab = stringP "    " <<- spaces in
  let raw_of_any = map ~f:(fun c -> Raw (String.of_char c)) any in
  let bullet = spaces <<- (oneOf "*-+" <<- section Uo |-| (sequence [ digits ; stringP "." ] <<- section Ol)) ->> charP ' ' 
             |> map ~f:(fun t x -> [List (t,[x])]) in
  let code =
    tab <<- until empty_line
    |> map ~f:
         begin
           fun x -> [Code (None,String.of_char_list x)]
         end
  in
  let table =
    let t_col = spaces <<- charP '|' ->> spaces in
    let table_line_col =
      t_col <<- map ~f:String.of_char_list (until (spaces <<- charP '|' |-| charP '\n')) in
    let table_line = repeat table_line_col ->> spaces ->> charP '\n' in
    let check_table = (*  ? ? ? -> *) repeat1 (t_col <<- repeat1 (charP '-')) ->> t_col ->> charP '\n' in
    let zip_with_num xs =
      let num = List.length xs in List.zip_exn xs (List.range 0 num)
    in
    let label = map table_line ~f:(fun x -> TMap.add_exn TMap.empty ~key:0 ~data:(List.rev x)) ->> check_table 
    in app (repeat table_line) ~f:
         begin map label ~f:
                 begin fun x xs ->
                 [ Table 
                     begin List.fold_left (zip_with_num xs) ~init:x ~f:
                             begin fun b (a,n) ->
                             TMap.add_exn b ~key:(n + 1) ~data:(List.rev a)
                             end
                     end
                 ] end
         end
  in
  let rec collect_raws =
    function
    | [] -> []
    | Raw x1 :: Raw x2 :: xs ->
       Raw (x1 ^ x2) :: xs
       |> collect_raws
    | List (t, x) :: xs ->
       List (t, List.map ~f:collect_raws x) :: collect_raws xs
    | x :: xs -> 
       x :: collect_raws xs
  in
  (* character wise parsing which associate with link parsing *)
  let rec iter _ : atom list parser =
  (* work around *)
    orP terminator 
      (lazy (consP (link |-| (charP '\\' <<- raw_of_any) |-| raw_of_any) (iter None)))
  in
  table |-| code |-| map ~f:collect_raws (app (iter None) ~f:bullet |-| (iter None))
          
let paragraph : paragraph_type parser =
  let line_sn = line (repeat (charP '#') <<- spaces <<- (charP '\n') ->> spaces) in
  let pre_header = (spaces <<- repeat1 (charP '#') ->> spaces) |> map ~f:List.length in
  let pre = map ~f:(fun x y z -> x, y, z) pre_header in
  let header_line = ((spaces -- repeat1 (charP '-') -- empty_line) <<- section 2)
                    |-| ((spaces -- repeat1 (charP '=') -- empty_line) <<- section 1) in
  let title = map ~f:(fun x y z -> y, x, z) (line empty_line) in
  let header = app header_line ~f:title |-| app line_sn ~f:pre in
  let sn_line_n = failP ((spaces -- (oneOf "#\n")) (* for header2 *)
                         |-| (until (charP '\n') -- charP '\n' -- spaces -- (oneOf "-="))) (* for header1 *) 
                  <<- line empty_line
  in
  let rec collect_lists =
    function
    | [] -> []
    | List (t1, x1) :: List (t2, x2) :: xs when equal_list_type t1 t2 ->
       List (t1, (List.map ~f:collect_lists (List.append x1 x2))) :: xs
       |> collect_lists
    | x :: xs -> 
       x :: collect_lists xs
  in
  let prim = repeat1 sn_line_n
             |> map ~f:
                  begin fun x ->
                  x
                  |> List.join
                  |> collect_lists
                  |> PrimParagraph
                  end
  in
  let to_par = map ~f:(fun (x, y, z) -> Paragraph { level=x ; header=y ; content=z }) in
  let add_header p hd = app ~f:hd (repeat p) |> to_par in
  let rec iter _ =
    orP prim
      (lazy (add_header (iter None) (header)))
  in iter None
                       
let rec extract_atom =
  function
  (* | HorizontalRule 
   * | Latex 
   * | AutoLink s  *)
  (* | Table tbl 
   * | RefLink { name ; id } *)
  (* | ImageLink { name ; url ; title }  *)
  | Link { name ; url } ->
     "<a href=" ^ url ^ ">" ^ name ^ "</a>"
  | List (Ol,atomss) ->
     let extract_line = fold_lump extract_atom in
     List.fold_left atomss ~init:"<ol>" ~f:(fun b a -> b ^ "<li>" ^ (extract_line a) ^ "</li>") ^ "</ol>"
  | List (Uo,atomss) ->
     let extract_line = fold_lump extract_atom in
     List.fold_left atomss ~init:"<ul>" ~f:(fun b a -> b ^ "<li>" ^ (extract_line a) ^ "</li>") ^ "</ul>"
  | Code (_, code) ->
     "<pre><code>" ^ code ^ "</code></pre>"
  | Raw s -> s
  | _ -> failwith "extract_atom"
          
let extract_line = fold_lump extract_atom 
                 
let equal_list ~equal a1 a2 = 
  List.zip a1 a2
  |> Option.map ~f:(List.fold_left ~init:true ~f:(fun r (a1,a2) -> equal a1 a2 && r))
  |> Option.value ~default:false
       
let rec equal_atom a1 a2 =
  match a1,a2 with
  | Raw s1, Raw s2
  | AutoLink s1, AutoLink s2 -> String.equal s1 s2
  | ImageLink { name ; url ; title; }, ImageLink { name=name' ; url=url' ; title=title'; } ->
     String.equal name name' && String.equal url url' && Option.equal String.equal title title'
  | Emphasis a1, Emphasis a2 ->
     let equal_atom_list = equal_list ~equal:equal_atom in
     equal_atom_list a1 a2
  | HorizontalRule , HorizontalRule  
  | Latex , Latex -> true
  | Ref r1, Ref r2 ->
     RMap.equal String.equal r1 r2
  | Table t1, Table t2 -> TMap.equal (List.equal ~equal:String.equal) t1 t2
  | Link { name; url }, Link { name=name'; url=url' } ->
     String.equal name name' && String.equal url url' 
  | RefLink { name ; id }, RefLink { name=name' ; id=id' } ->
     String.equal name name' && String.equal id id' 
  | List (t1,a1), List (t2,a2) when equal_list_type t1 t2 ->
     let equal_atom_list = equal_list ~equal:equal_atom in
     begin match List.fold2 a1 a2 ~init:true ~f:(fun r a1 a2 -> equal_atom_list a1 a2 && r) with
     | Ok b -> b
     | Unequal_lengths -> false
     end
  | Code (c1,s1), Code (c2,s2) when Option.equal equal_code_type c1 c2 ->
     String.equal s1 s2
  | _, _ -> false

let rec equal p1 p2 =
  match p1,p2 with
  | Paragraph { level ; header ; content }, Paragraph { level=level' ; header=header' ; content=content' } ->
     Int.equal level level' && equal_list ~equal:equal_atom header header' && equal_list ~equal:equal content content'
  | PrimParagraph a1, PrimParagraph a2 ->
     equal_list ~equal:equal_atom a1 a2
  | BlockQuote p1, BlockQuote p2 ->
     equal p1 p2
  | _, _ -> false
  
let repeats n =
  let rec iter n =
  if n <= 0 
  then []
  else '#' :: iter (n - 1)
  in iter n |> String.of_char_list
  
let rec extract_paragraph =
  function
  | Paragraph { level ; header ; content } ->
     let extract_paragraphs = fold_lump extract_paragraph in
     if level <= 0
     then "<p>" ^ extract_paragraphs content ^ "</p>"
     else if level <= 6
     then "<h" ^ Int.to_string level ^ ">" ^ "<p>" ^
            extract_line header
            ^ "</p>" ^ "</h" ^ Int.to_string level ^ ">"
     else
       repeats level ^ "<p>" ^
         fold_lump string_of_atom header ^ "</p>"
  | PrimParagraph atoms ->
     "<p>" ^ extract_line atoms ^ "</p>"
  | BlockQuote paragraph -> 
     "<blockquote>" ^ extract_paragraph paragraph ^ "</blockquote>"

 let parse src =
   let src' = normalize (src ^ "\n") in
   let res, _ = run (repeat (paragraph ->> empty_line)) src' in
   match res with
   | First res -> res |> List.rev
   | Second _ ->
      failwith "parse error: markdown.ml"
