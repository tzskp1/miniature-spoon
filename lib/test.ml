module M = Markdown
module P = Parser
open Base
   
let print t = List.iter t ~f:(fun x -> M.string_of_paragraph x |> Stdio.print_endline)
       
let%test "normalize1" =
  String.equal "\n\n" (P.normalize "\n \n")
  
let%test "normalize2" =
  String.equal "\n\n\n" (P.normalize "\n   \n \n")
  
let%test "normalize3" =
  String.equal "test   \n\n\n" (P.normalize "test   \n   \n \n")
   
let eq a a' = List.equal ~equal:M.equal a a'
            
let%test "test_escape1" =
  eq (M.parse "\\1. omg \n")
    [ M.PrimParagraph [ M.Raw "1. omg" ] ]
                     
let%test "test_paragraph1" =
  eq (M.parse "## This is a header.\n This is a content.\n")
    [ M.Paragraph
       {level = 2; header = [M.Raw "This is a header."];
        content = [M.PrimParagraph [M.Raw "This is a content." ]]} ]
  
let%test "test_paragraph2" =
  eq (M.parse "## This is a header.\n ### This is a sub header. ### \n in sub \n")
    [ M.Paragraph
           {level = 2; header = [M.Raw "This is a header."];
            content =
              [M.Paragraph
                 {level = 3; header = [M.Raw "This is a sub header. "];
                  content = [M.PrimParagraph [M.Raw "in sub"]]}]} ]
    
let%test "test_paragraph2'" =
  eq (M.parse "## This is a header.\n ### ### This is a sub header. ### \n nth23 \n")
       [ M.Paragraph
           {level = 2; header = [M.Raw "This is a header."];
            content =
              [M.Paragraph
                 {level = 3; header = [M.Raw "### This is a sub header. "];
                  content = [M.PrimParagraph [M.Raw "nth23"]]}]} ]
  
let%test "test_paragraph3" =
  eq (M.parse "### This is a sub header. ### \n nth23 \n")
       [ M.Paragraph
           {level = 3; header = [M.Raw "This is a sub header. "];
            content = [M.PrimParagraph [M.Raw "nth23"]]} ]
  
(* --here--  *)
let%test "test_paragraph4" =
  eq (M.parse "paragraph1\n\nparagraph2\n")
    [ M.PrimParagraph [M.Raw "paragraph1"]; M.PrimParagraph [M.Raw "paragraph2"] ]
  
let%test "test_link1" =
  eq (M.parse "[an example][id]\n \n")
       [ M.PrimParagraph
          [M.RefLink {name = "an example"; id = "id"}] ]
  
let%test "test_atom1" =
  eq (M.parse "This is [an example] [id] reference-style link.\n \n")
       [ M.PrimParagraph
           [M.Raw "This is"; M.RefLink {name = "an example"; id = "id"};
            M.Raw "reference-style link."] ]
  
let%test "test_raw1" =
  eq (M.parse "This is a content.\n \n")
    [ M.PrimParagraph [M.Raw "This is a content."] ]
  
let%test "test_header1" =
  eq (M.parse "### This is a title.\n \n")
       [ M.Paragraph
          {level = 3; header = [M.Raw "This is a title."];
           content = [] } ]
  
let%test "test_header1'" =
  eq (M.parse "This is an H1 \n ========== \n \n")
       [ M.Paragraph
           {level = 1; header = [M.Raw "This is an H1"];
            content = []} ]

let%test "test_list1" =
  let xs = [ [M.Raw "one"] ; [M.Raw "two"] ] in
  eq (M.parse  "1. one \n 2. two \n")
    [ M.PrimParagraph [M.List (M.Ol, xs)] ]
  
let%test "test_code1" =
  eq (M.parse "    echo 'hello'\n")
    [ M.PrimParagraph [M.Code (None, "echo 'hello'")] ]
  
module TMap = Markdown_parser_extractor.Markdown.TMap
let%test "test_table1" =
  let label = TMap.add_exn TMap.empty ~key:0 ~data:["Plugins"; "Plugin"; "README"; ""] in
  let table = TMap.add_exn label ~key:1 ~data:["Dropbox"; "[plugins/dropbox/README.md]"; ""] in
  eq (M.parse "| Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n")
    [ M.PrimParagraph [M.Table table] ]
