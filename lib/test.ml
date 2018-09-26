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
    [ M.Paragraph { header=None; contents=[ M.Raw "1. omg" ] } ]
                     
let%test "test_paragraph1" =
  eq (M.parse "## This is a header.\n This is a contents.\n")
    [M.Paragraph
       { header=Some(2,[M.Raw "This is a header."])
         ; contents=[M.Raw "This is a contents." ] } ]
  
let%test "test_paragraph2" =
  eq (M.parse "## This is a header.\n ### This is a sub header. ### \n in sub \n")
    [ M.Paragraph
           {header = Some(2,[M.Raw "This is a header."]);
            contents = [] } ;
      M.Paragraph
        {header = Some(3,[M.Raw "This is a sub header. "]);
         contents = [M.Raw "in sub"] } ]
    
let%test "test_paragraph2'" =
  eq (M.parse "## This is a header.\n ### ### This is a sub header. ### \n nth23 \n")
    [ M.Paragraph
           {header = Some(2,[M.Raw "This is a header."]);
            contents = [] };
      M.Paragraph
         {header = Some(3,[M.Raw "### This is a sub header. "]);
          contents = [M.Raw "nth23"] } ]
  
let%test "test_paragraph3" =
  eq (M.parse "### This is a sub header. ### \n nth23 \n")
       [ M.Paragraph
           {header = Some(3,[M.Raw "This is a sub header. "]);
            contents = [M.Raw "nth23"] }]
  
let%test "test_paragraph4" =
  eq (M.parse "paragraph1\n\nparagraph2\n")
    [ M.Paragraph { header=None; contents=[M.Raw "paragraph1"] }; M.Paragraph { header=None; contents=[M.Raw "paragraph2"] } ]
  
let%test "test_link1" =
  eq (M.parse "[an example][id]\n \n")
       [ M.Paragraph
           { header=None; contents=[M.RefLink {name = "an example"; id = "id"}]} ]
  
let%test "test_atom1" =
  eq (M.parse "This is [an example] [id] reference-style link.\n \n")
       [ M.Paragraph
           { header=None; contents= 
                            [M.Raw "This is"; M.RefLink {name = "an example"; id = "id"};
                             M.Raw "reference-style link."] } ]
  
let%test "test_raw1" =
  eq (M.parse "This is a contents.\n \n")
    [ M.Paragraph
        { header=None; contents= 
        [M.Raw "This is a contents."]} ]
  
let%test "test_header1" =
  eq (M.parse "### This is a title.\n \n")
       [ M.Paragraph
          {header = Some(3,[M.Raw "This is a title."]);
           contents = [] } ]
  
let%test "test_header1'" =
  eq (M.parse "This is an H1 \n ========== \n \n")
       [ M.Paragraph
           {header = Some(1,[M.Raw "This is an H1"]);
            contents = []} ]

let%test "test_list1" =
  let xs = [ [M.Raw "one"] ; [M.Raw "two"] ] in
  eq (M.parse  "1. one \n 2. two \n")
    [ M.Paragraph { header=None; contents=[M.List (M.Ol, xs)] } ]
  
let%test "test_code1" =
  eq (M.parse "    echo 'hello'\n")
    [ M.Paragraph
        { header=None; contents=[M.Code (None, "echo 'hello'")]} ]
  
module TMap = Markdown_parser_extractor.Markdown.TMap
let%test "test_table1" =
  let label = TMap.add_exn TMap.empty ~key:0 ~data:["Plugins"; "Plugin"; "README"; ""] in
  let table = TMap.add_exn label ~key:1 ~data:["Dropbox"; "[plugins/dropbox/README.md]"; ""] in
  eq (M.parse "| Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n")
    [ M.Paragraph { header=None; contents=[M.Table table] } ]

let%test "test_table1'" =
  let () = print (M.parse "# header \n | Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n") in
  let label = TMap.add_exn TMap.empty ~key:0 ~data:["Plugins"; "Plugin"; "README"; ""] in
  let table = TMap.add_exn label ~key:1 ~data:["Dropbox"; "[plugins/dropbox/README.md]"; ""] in
  eq (M.parse "# header \n | Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n")
    [ M.Paragraph { header=Some(1,[ M.Raw "header" ]); contents= [ M.Table table ] } ]
