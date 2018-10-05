open Parser
open Core
module TMap = Map.Make(Int)
module RMap = Map.Make(String)
open Ast
               
let fold_list piece =
  List.fold_left ~init:"" ~f:(fun b a -> b ^ (piece a))
  
(* due to blockparagraph e.g.
before
> 1
> 2
after
> 1
2
*)
let normalize' src =
  let count hd =
    List.count ~f:(Char.equal '>') (String.to_list hd)
  in
  let rec collect =
    function
    | [ Str.Delim hd1; Str.Text r1 ] :: xs ->
       begin match collect xs with
       | [ Str.Delim hd2; Str.Text r2 ] :: r ->
          if count hd1 = count hd2
          then [ Str.Delim hd1 ; Str.Text r1 ] :: [ Str.Text r2 ] :: r
          else [ Str.Delim hd1 ; Str.Text r1 ] :: [ Str.Delim hd2 ; Str.Text r2 ] :: r
       | r -> [ Str.Delim hd1 ; Str.Text r1 ] :: r
       end
    | [] -> []
    | x :: xs ->
       x :: collect xs
  in
  let rec concat =
    function
    | Str.Delim c :: xs 
    | Str.Text c :: xs ->
       c ^ concat xs
    | [] -> ""
  in
  let pat = Str.regexp "^ *>+" in
  let ret = Str.regexp "\n" in
  Str.split ret src 
  |> List.map ~f:(Str.full_split pat)
  |> collect
  |> List.map ~f:concat
  |> List.fold_right ~init:"" ~f:(fun a b -> a ^ "\n" ^ b)
    
let surounded_string l r =
  (* work around *)
  let rec iter _ =
    orP (r <<- section []) (lazy (consP any (iter None)))
  in 
  l <<- iter None |> map ~f:String.of_char_list
    
let link : span parser =
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
               
let table =
  let t_col = spaces -- charP '|' -- spaces in
  let table_line_col =
    map ~f:String.of_char_list (until (spaces -- (charP '|' |-| charP '\n') -- spaces)) in
  let table_line = t_col <<- repeat1 (table_line_col ->> t_col) ->> (spaces -- charP '\n') in
  let check_table = repeat1 (t_col <<- repeat1 (charP '-')) ->> (t_col -- charP '\n') in
  let zip_with_num xs =
    let num = List.length xs in List.zip_exn xs (List.range 0 num)
  in
  let label = map table_line ~f:(fun x -> TMap.add_exn TMap.empty ~key:0 ~data:(List.rev x)) ->> check_table 
  in app (repeat table_line) ~f:
       begin map label ~f:
               begin fun x xs ->
               Table 
                 begin List.fold_left (zip_with_num xs) ~init:x ~f:
                         begin fun b (a,n) ->
                         TMap.add_exn b ~key:(n + 1) ~data:(List.rev a)
                         end
                 end
               end
       end
   
let tab = stringP "    " <<- spaces 
        
let code =
  let subst = Str.global_substitute (Str.regexp "\n^ +") (Fn.const "\n")
  in
  tab <<- until (spaces -- checkP (stringP "\n\n"))
  |> map ~f:
       begin
         fun x -> Code (None, Fn.compose subst String.of_char_list x)
       end
  
let raw_of_any = map ~f:(fun c -> Raw (String.of_char c)) any
               
let bullet =
  spaces <<- (oneOf "*-+" <<- section Uo |-| (sequence [ digits ; stringP "." ] <<- section Ol)) ->> charP ' ' 
  |> map ~f:(fun t x -> List (t,[x]))
  
let emphasis line =
  spaces <<- stringP "**" <<- (line (stringP "**" -- spaces <<- section []))
  |> map ~f:(fun x -> StrongEmphasis x)
  |-|
    begin
      spaces <<- stringP "*" <<- (line (stringP "*" -- spaces <<- section []))
      |> map ~f:(fun x -> Emphasis x)
    end
  
let rec collect =
  function
  | [] -> []
  | Raw x1 :: Raw x2 :: xs ->
     Raw (x1 ^ x2) :: xs
     |> collect
  | List (t1, x1) :: List (t2, x2) :: xs when equal_list_type t1 t2 ->
     List (t1, (List.map ~f:collect (List.append x1 x2))) :: xs
     |> collect
  | List (t, x) :: xs ->
     List (t, List.map ~f:collect x) :: collect xs
  | Emphasis x :: xs ->
     Emphasis (collect x) :: collect xs
  | StrongEmphasis x :: xs ->
     StrongEmphasis (collect x) :: collect xs
  | x :: xs -> 
     x :: collect xs
  
let lines (terminator : 'a list parser) : span list parser = 
  (* character wise parsing which associate with link/list/code/table parsing *)
  let rec iter (terminator : 'a list parser) : span list parser =
  (* work around *)
    orP (terminator <<- section [])
      begin lazy (consP (link 
                         |-| table
                         |-| code
                         |-| emphasis iter
                         |-| app ~f:bullet (iter (spaces <<- (charP '\n') <<- section [])) (* list *)
                         |-| (charP '\\' <<- raw_of_any) (* escaping *)
                         |-| raw_of_any)
                    (iter terminator))
      end
  in
  map ~f:collect (iter terminator)
  
let header_line = (spaces -- repeat1 (charP '-') -- empty_line) <<- section 2
                  |-| ((spaces -- repeat1 (charP '=') -- empty_line) <<- section 1)
  
let title = lines (empty_line <<- section [])
            |> map ~f:(fun x y -> Some (y, x))
            |> fun f -> app header_line ~f:f
  
let pre_header = spaces <<- repeat1 (charP '#') ->> spaces
                 |> map ~f:(fun x y -> Some (List.length x, y))
                 |> fun f -> app ~f:f
                               begin
                                 lines (repeat (charP '#') -- spaces -- (charP '\n') -- spaces)
                               end
                
let paragraph = app
                  begin
                    lines (spaces -- (stringP "\n\n" |-| checkP (stringP "#") |-| checkP (stringP "\n>")) <<- section [])
                  end ~f:
                  begin map ~f:(fun header x -> Paragraph { header=header; contents=x })
                          begin
                            title
                            |-| pre_header
                            |-| section None
                          end
                  end
                ->> tryP (repeat (charP '\n'))
              
let rec blockquote _ =
  let (<<-) p1 p2 = bind ~f:(fun _ -> Lazy.force p2) p1 in
  map ~f:(fun p -> BlockQuote p)
    ((repeat (charP '\n') -- charP '>' -- spaces) <<- lazy (blockquote None |-| paragraph))
  
let paragraphs : paragraph_type list parser =
  repeat1 (blockquote None |-| paragraph)
                       
let rec extract_span =
  function
  (* | HorizontalRule 
   * | Latex 
   * | AutoLink s
   * | Table tbl 
   * | RefLink { name ; id }
   * | ImageLink { name ; url ; title } *)
  | Link { name ; url } ->
     "<a href=" ^ url ^ ">"
     ^ name
     ^ "</a>"
  | List (Ol, spanss) ->
     List.fold_left spanss ~init:"<ol>"
       ~f:(fun b a -> b ^ "<li>" ^ fold_list extract_span a ^ "</li>")
     ^ "</ol>"
  | List (Uo, spanss) ->
     List.fold_left spanss ~init:"<ul>"
       ~f:(fun b a -> b ^ "<li>" ^ fold_list extract_span a ^ "</li>")
     ^ "</ul>"
  | Code (_, code) ->
     "<pre><code>" ^
       code
       ^ "</code></pre>"
  | Emphasis spans ->
     "<em>" ^
       fold_list extract_span spans
     ^ "</em>"
  | StrongEmphasis spans ->
     "<strong>" ^
       fold_list extract_span spans
     ^ "</strong>"
  | Raw s -> s
  | _ -> failwith "extract_span"
          
let extract_line = fold_list extract_span 
                 
let repeats n =
  List.range 0 n
  |> List.map ~f:(Fn.const '#')
  |> String.of_char_list
  
let rec extract_paragraph =
  function
  | Paragraph { header ; contents } ->
     begin match header with
     | Some(level, header) -> 
        if level <= 0
        then "<p>" ^ extract_line contents ^  "</p>"
        else if level <= 6
        then "<h" ^ Int.to_string level ^ ">" ^ "<p>" ^
               extract_line header
               ^ "</p>" ^ "</h" ^ Int.to_string level ^ ">"
               ^ "<p>" ^ extract_line contents ^ "</p>"
        else
          repeats level ^ "<p>" ^ extract_line contents ^ "</p>"
     | None -> 
        "<p>" ^ extract_line contents ^  "</p>"
     end
  | BlockQuote paragraph -> 
     "<blockquote>" ^ extract_paragraph paragraph ^ "</blockquote>"

let extract (src : paragraph_type list) : string = fold_list extract_paragraph src ^ "\n"

let parse (src : string) : paragraph_type list =
  let src' = Fn.compose normalize' normalize (src ^ "\n\n") in
  let res, _ = run paragraphs src' in
  match res with
  | First res -> res |> List.rev
  | Second Fail ->
     failwith "parse error: markdown.ml"
