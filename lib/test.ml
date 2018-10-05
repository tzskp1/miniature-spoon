module M = Markdown
module P = Parser
open Core
   
let print t = List.iter t ~f:(fun x -> Fn.compose string_of_sexp Ast.sexp_of_paragraph_type x |> Stdio.print_endline)
       
let%test "normalize1" =
  String.equal "\n\n" (P.normalize "\n \n")
  
let%test "normalize2" =
  String.equal "\n\n\n" (P.normalize "\n   \n \n")
  
let%test "normalize3" =
  String.equal "test   \n\n\n" (P.normalize "test   \n   \n \n")
   
let eq = List.equal ~equal:(fun a b -> Sexp.equal (Ast.sexp_of_paragraph_type a) (Ast.sexp_of_paragraph_type b))
            
let%test "test_escape1" =
  eq (M.parse "\\1. omg \n")
    [ Ast.Paragraph { header=None; contents=[ Ast.Raw "1. omg" ] } ]
                     
let%test "test_paragraph1" =
  eq (M.parse "## This is a header.\n This is a contents.\n")
    [Ast.Paragraph
       { header=Some(2,[Ast.Raw "This is a header."])
         ; contents=[Ast.Raw "This is a contents." ] } ]
  
let%test "test_paragraph1'" =
  eq (M.parse "# This is a header.\n This is a contents.\n")
    [Ast.Paragraph
       { header=Some(1,[Ast.Raw "This is a header."])
         ; contents=[Ast.Raw "This is a contents." ] } ]
  
let%test "test_paragraph2" =
  eq (M.parse "## This is a header.\n ### This is a sub header. ### \n in sub \n")
    [ Ast.Paragraph
           {header = Some(2,[Ast.Raw "This is a header."]);
            contents = [] } ;
      Ast.Paragraph
        {header = Some(3,[Ast.Raw "This is a sub header. "]);
         contents = [Ast.Raw "in sub"] } ]
    
let%test "test_paragraph2'" =
  eq (M.parse "## This is a header.\n ### ### This is a sub header. ### \n nth23 \n")
    [ Ast.Paragraph
           {header = Some(2,[Ast.Raw "This is a header."]);
            contents = [] };
      Ast.Paragraph
         {header = Some(3,[Ast.Raw "### This is a sub header. "]);
          contents = [Ast.Raw "nth23"] } ]
  
let%test "test_paragraph3" =
  eq (M.parse "### This is a sub header. ### \n nth23 \n")
       [ Ast.Paragraph
           {header = Some(3,[Ast.Raw "This is a sub header. "]);
            contents = [Ast.Raw "nth23"] }]
  
let%test "test_paragraph4" =
  eq (M.parse "paragraph1\n\nparagraph2\n")
    [ Ast.Paragraph { header=None; contents=[Ast.Raw "paragraph1"] }; Ast.Paragraph { header=None; contents=[Ast.Raw "paragraph2"] } ]
  
let%test "test_paragraph4'" =
  eq (M.parse "paragraph1\nparagraph2\n")
    [ Ast.Paragraph { header=None; contents=[Ast.Raw "paragraph1\nparagraph2"] } ]
  
let%test "test_link1" =
  eq (M.parse "[an example][id]\n \n")
       [ Ast.Paragraph
           { header=None; contents=[Ast.RefLink {name = "an example"; id = "id"}]} ]
  
let%test "test_span1" =
  eq (M.parse "This is [an example] [id] reference-style link.\n \n")
       [ Ast.Paragraph
           { header=None; contents= 
                            [Ast.Raw "This is"; Ast.RefLink {name = "an example"; id = "id"};
                             Ast.Raw "reference-style link."] } ]
  
let%test "test_raw1" =
  eq (M.parse "This is a contents.\n \n")
    [ Ast.Paragraph
        { header=None; contents= 
        [Ast.Raw "This is a contents."]} ]
  
let%test "test_header1" =
  eq (M.parse "### This is a title.\n \n")
       [ Ast.Paragraph
          {header = Some(3,[Ast.Raw "This is a title."]);
           contents = [] } ]
  
let%test "test_header1'" =
  eq (M.parse "This is an H1 \n ========== \n \n")
       [ Ast.Paragraph
           {header = Some(1,[Ast.Raw "This is an H1"]);
            contents = []} ]

let%test "test_list1" =
  let xs = [ [Ast.Raw "one"] ; [Ast.Raw "two"] ] in
  eq (M.parse  "1. one \n 2. two \n")
    [ Ast.Paragraph { header=None; contents=[Ast.List (Ast.Ol, xs)] } ]
  
let%test "test_code1" =
  eq (M.parse "    echo 'hello'\n")
    [ Ast.Paragraph
        { header=None; contents=[Ast.Code (None, "echo 'hello'")]} ]
  
let%test "test_blockquote1" =
  eq (M.parse "> test")
    [ Ast.BlockQuote (Ast.Paragraph { header=None; contents=[Ast.Raw "test"]}) ]
  
let%test "test_blockquote2" =
  eq (M.parse ">> test")
    [ Ast.BlockQuote (Ast.BlockQuote (Ast.Paragraph { header=None; contents=[Ast.Raw "test"]})) ]
  
let%test "test_blockquote3" =
  eq (M.parse "> line1 \n line2")
    [ Ast.BlockQuote (Ast.Paragraph { header=None; contents=[Ast.Raw "line1 \n line2"]}) ]
  
let%test "test_blockquote3'" =
  eq (M.parse "> line1 \n> line2")
    [ Ast.BlockQuote (Ast.Paragraph { header=None; contents=[Ast.Raw "line1 \n line2"]}) ]
  
let%test "test_blockquote3''" =
  eq (M.parse "> line1 \n\n> line2")
    [Ast.BlockQuote
       (Ast.Paragraph
          {header = None;
           contents = [Ast.Raw "line1"]});
    Ast.BlockQuote
       (Ast.Paragraph
          {header = None;
           contents = [Ast.Raw "line2"]})]
  
let%test "test_blockquote4" =
  eq (M.parse "> line1 \n>> line2")
    [Ast.BlockQuote
       (Ast.Paragraph
          {header = None;
           contents = [Ast.Raw "line1"]});
     Ast.BlockQuote
       (Ast.BlockQuote
          (Ast.Paragraph
             {header = None;
              contents = [Ast.Raw "line2"]}))]
  
module TMap = Ast.TMap
let%test "test_table1" =
  let label = TMap.add_exn TMap.empty ~key:0 ~data:["Plugins"; "Plugin"; "README";] in
  let table = TMap.add_exn label ~key:1 ~data:["Dropbox"; "[plugins/dropbox/README.md]";] in
  eq (M.parse "| Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n")
    [ Ast.Paragraph { header=None; contents=[Ast.Table table] } ]

let%test "test_table1'" =
  let label = TMap.add_exn TMap.empty ~key:0 ~data:["Plugins"; "Plugin"; "README";] in
  let table = TMap.add_exn label ~key:1 ~data:["Dropbox"; "[plugins/dropbox/README.md]";] in
  eq (M.parse "# header \n | Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n")
    [ Ast.Paragraph { header=Some(1,[ Ast.Raw "header" ]); contents= [ Ast.Table table ] } ]

let%test "test_emphasis1" =
  eq (M.parse "*test*\n")
    [ Ast.Paragraph { header=None; contents=[ Ast.Emphasis [ Ast.Raw "test" ] ] } ]
    
let%test "test_emphasis2" =
  eq (M.parse "**test**\n")
    [ Ast.Paragraph { header=None; contents=[ Ast.StrongEmphasis [ Ast.Raw "test" ] ] } ]
