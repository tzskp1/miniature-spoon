open Parser
open Core
module TMap = Map.Make(Int)
module RMap = Map.Make(String)
   
type code_type =
  | Shell
[@@deriving sexp]
  
let equal_code_type t1 t2 =
  match t1,t2 with
  | Shell, Shell -> true
  
type list_type =
  | Uo
  | Ol
[@@deriving sexp]

let equal_list_type t1 t2 =
  match t1,t2 with
  | Uo, Uo | Ol, Ol -> true
  | _, _ -> false
  
type span =
  | AutoLink of string (* TODO *)
  | ImageLink of { name : string ; url : string ; title : string option } (* TODO *)
  | Emphasis of span list (* TODO *)
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
  
let string_of_span = Fn.compose string_of_sexp sexp_of_span
                   
let string_of_paragraph = Fn.compose string_of_sexp sexp_of_paragraph_type
    
let fold_lump piece =
  List.fold_left ~init:"" ~f:(fun b a -> b ^ (piece a))
  
(* due to blockparagraph *)
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
               [ Table 
                   begin List.fold_left (zip_with_num xs) ~init:x ~f:
                           begin fun b (a,n) ->
                           TMap.add_exn b ~key:(n + 1) ~data:(List.rev a)
                           end
                   end
               ] end
       end
   
let tab = stringP "    " <<- spaces 
        
let code =
  tab <<- until (spaces -- stringP "\n\n")
  |> map ~f:
       begin
         fun x -> [Code (None,String.of_char_list x)]
       end
  
let lines terminator : span list parser = 
  let raw_of_any = map ~f:(fun c -> Raw (String.of_char c)) any in
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
  let rec iter _ : span list parser =
  (* work around *)
    orP (terminator <<- section [])
      (lazy (consP (link |-| (charP '\\' <<- raw_of_any) (* escaping *) |-| raw_of_any) (iter None)))
  in
  map ~f:collect_raws (iter None)
                       
let list =
  let bullet = spaces <<- (oneOf "*-+" <<- section Uo |-| (sequence [ digits ; stringP "." ] <<- section Ol)) ->> charP ' ' 
             |> map ~f:(fun t x -> [List (t,[x])]) in
  let rec collect_lists =
    function
    | [] -> []
    | List (t1, x1) :: List (t2, x2) :: xs when equal_list_type t1 t2 ->
       List (t1, (List.map ~f:collect_lists (List.append x1 x2))) :: xs
       |> collect_lists
    | x :: xs -> 
       x :: collect_lists xs
  in
  repeat1 (app (lines (spaces <<- (charP '\n'))) ~f:bullet)
  |> map ~f:(Fn.compose collect_lists (Fn.compose List.join List.rev))
  
let paragraphs : paragraph_type list parser =
  let pre_header = spaces <<- repeat1 (charP '#') ->> spaces
                   |> map ~f:(fun x y -> Some (List.length x, y))
                   |> fun f -> app (lines (repeat (charP '#') -- spaces -- (charP '\n') -- spaces)) ~f:f
  in
  let header_line = (spaces -- repeat1 (charP '-') -- empty_line) <<- section 2
                    |-| ((spaces -- repeat1 (charP '=') -- empty_line) <<- section 1)
  in
  let title = lines empty_line
              |> map ~f:(fun x y -> Some (y, x))
              |> fun f -> app header_line ~f:f
  in
  let paragraph = app
                    begin
                      table |-| code |-| list |-| lines (spaces -- (stringP "\n\n" |-| checkP (stringP "#") |-| checkP (stringP "\n>")))
                    end ~f:
                    begin map (title |-| pre_header |-| section None) ~f:
                            begin fun header x ->
                            Paragraph { header=header; contents=x }
                            end
                    end
                  ->> tryP (repeat (charP '\n'))
  in
  let rec blockquote _ =
    let (<<-) p1 p2 = bind ~f:(fun _ -> Lazy.force p2) p1 in
    map ~f:(fun p -> BlockQuote p)
      ((repeat (charP '\n') -- charP '>' -- spaces) <<- lazy (blockquote None |-| paragraph))
  in
  repeat1 (blockquote None |-| paragraph)
                       
let rec extract_span =
  function
  (* | HorizontalRule 
   * | Latex 
   * | AutoLink s  *)
  (* | Table tbl 
   * | RefLink { name ; id } *)
  (* | ImageLink { name ; url ; title }  *)
  | Link { name ; url } ->
     "<a href=" ^ url ^ ">" ^ name ^ "</a>"
  | List (Ol,spanss) ->
     let extract_line = fold_lump extract_span in
     List.fold_left spanss ~init:"<ol>" ~f:(fun b a -> b ^ "<li>" ^ (extract_line a) ^ "</li>") ^ "</ol>"
  | List (Uo,spanss) ->
     let extract_line = fold_lump extract_span in
     List.fold_left spanss ~init:"<ul>" ~f:(fun b a -> b ^ "<li>" ^ (extract_line a) ^ "</li>") ^ "</ul>"
  | Code (_, code) ->
     "<pre><code>" ^ code ^ "</code></pre>"
  | Raw s -> s
  | _ -> failwith "extract_span"
          
let extract_line = fold_lump extract_span 
                 
let equal_list ~equal a1 a2 = 
  List.zip a1 a2
  |> Option.map ~f:(List.fold_left ~init:true ~f:(fun r (a1,a2) -> equal a1 a2 && r))
  |> Option.value ~default:false
       
let rec equal_span a1 a2 =
  match a1,a2 with
  | Raw s1, Raw s2
  | AutoLink s1, AutoLink s2 -> String.equal s1 s2
  | ImageLink { name ; url ; title; }, ImageLink { name=name' ; url=url' ; title=title'; } ->
     String.equal name name' && String.equal url url' && Option.equal String.equal title title'
  | Emphasis a1, Emphasis a2 ->
     let equal_span_list = equal_list ~equal:equal_span in
     equal_span_list a1 a2
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
     let equal_span_list = equal_list ~equal:equal_span in
     begin match List.fold2 a1 a2 ~init:true ~f:(fun r a1 a2 -> equal_span_list a1 a2 && r) with
     | Ok b -> b
     | Unequal_lengths -> false
     end
  | Code (c1,s1), Code (c2,s2) when Option.equal equal_code_type c1 c2 ->
     String.equal s1 s2
  | _, _ -> false

let rec equal p1 p2 =
  match p1,p2 with
  | Paragraph { header ; contents }, Paragraph { header=header' ; contents=contents' } ->
     begin match header, header' with
     | Some(level, header), Some(level', header') ->
        Int.equal level level' && equal_list ~equal:equal_span header header' && equal_list ~equal:equal_span contents contents'
     | None, None -> equal_list ~equal:equal_span contents contents'
     | _, _ -> false
     end
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
  | Paragraph { header ; contents } ->
     begin match header with
     | Some(level, header) -> 
        if level <= 0
        then "<p>" ^ extract_line contents ^  "</p>"
        else if level <= 6
        then "<h" ^ Int.to_string level ^ ">" ^ "<p>" ^
               extract_line header
               ^ "</p>" ^ "</h" ^ Int.to_string level ^ ">"
        else
          repeats level ^ "<p>"
          ^ extract_line contents ^ "</p>"
     | None -> 
        "<p>" ^extract_line contents ^  "</p>"
     end
  | BlockQuote paragraph -> 
     "<blockquote>" ^ extract_paragraph paragraph ^ "</blockquote>"

let extract src = fold_lump extract_paragraph src ^ "\n"

 let parse src =
   let src' = normalize' (normalize (src ^ "\n\n")) in
   let res, _ = run paragraphs src' in
   match res with
   | First res -> res |> List.rev
   | Second Fail ->
      failwith "parse error: markdown.ml"
