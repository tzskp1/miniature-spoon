module M = Markdown
module P = Parser 
open Base
   
let eq (a,b) (a',b') = (Either.equal M.equal P.fail_equal) a a' && String.equal b b' 
                     
let%test "test_paragraph1" =
  eq (P.run M.paragraph "## This is a header.\n This is a content.\n \n")
    (Either.First (M.Paragraph
                     {level = 2; header = [M.Raw "This is a header."];
                      content = [M.PrimParagraph [M.Raw "This is a content."; M.Raw " "]]}), "") 
  
let%test "test_paragraph2" =
  eq (P.run M.paragraph "## This is a header.\n ### This is a sub header. ### \n nth23 \n")
    (Either.First
        (M.Paragraph
           {level = 2; header = [M.Raw "This is a header."];
            content =
              [M.Paragraph
                 {level = 3; header = [M.Raw "This is a sub header. "];
                  content = [M.PrimParagraph [M.Raw "nth23 "]]}]}), "")
    
let%test "test_paragraph2'" =
  eq (P.run M.paragraph "## This is a header.\n ### ### This is a sub header. ### \n nth23 \n")
    (Either.First
       (M.Paragraph
          {level = 2; header = [M.Raw "This is a header."];
           content =
             [M.Paragraph
                {level = 3; header = [M.Raw "### This is a sub header. "];
                 content = [M.PrimParagraph [M.Raw "nth23 "]]}]}), "") 
  
let%test "test_paragraph3" =
  eq (P.run M.paragraph "### This is a sub header. ### \n nth23 \n")
    (Either.First
       (M.Paragraph
          {level = 3; header = [M.Raw "This is a sub header. "];
           content = [M.PrimParagraph [M.Raw "nth23 "]]}), "")
  
let%test "test_link1" =
  eq (P.run M.paragraph "[an example][id]\n \n")
    (Base.Either.First
       (M.PrimParagraph
          [M.RefLink {name = "an example"; id = "id"}; M.Raw " "]), "")
  
let%test "test_atom1" =
  eq (P.run M.paragraph "This is [an example] [id] reference-style link.\n \n")
    (Either.First
       (M.PrimParagraph
          [M.Raw "This is"; M.RefLink {name = "an example"; id = "id"};
           M.Raw "reference-style link."; M.Raw " "]), "")
  
let%test "test_raw1" =
  eq (P.run M.paragraph "This is a content.\n \n")
  (Base.Either.First
    (M.PrimParagraph [M.Raw "This is a content."; M.Raw " "]), "")
  
let%test "test_header1" =
  eq (P.run M.paragraph "### This is a title.\n \n")
    (Base.Either.First
       (M.Paragraph
          {level = 3; header = [M.Raw "This is a title."];
           content = [M.PrimParagraph []]}), "")

  
let%test "test_list1" =
  let xs = [ [M.Raw "one "] ; [M.Raw "two "] ] in
  eq (P.run M.paragraph "1. one \n 2. two \n")
    ((Either.First (M.PrimParagraph [M.List (M.Ol, xs)])), "")
  
let%test "test_code1" =
  eq (P.run M.paragraph "    echo 'hello'\n")
    (Base.Either.First (M.PrimParagraph [M.Code (None, "echo 'hello'\n")]), "")
  
module TMap = Markdown_parser_extractor.Markdown.TMap
let%test "test_table1" =
  let label = TMap.add_exn TMap.empty ~key:0 ~data:["Plugins"; "Plugin"; "README"; ""] in
  let table = TMap.add_exn label ~key:1 ~data:["Dropbox"; "[plugins/dropbox/README.md"; "";] in
  eq (P.run M.paragraph "| Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n")
    (Base.Either.First (M.PrimParagraph [M.Table table]), "")
                
let do_test data =
  let res, rest = P.run M.paragraph data in
  begin match res with
  | First r ->
     M.string_of_paragraph r
     |> ((^) "Success: ")
  | _ -> "Fail: " ^ rest
  end |> Stdio.print_endline
  
let () = do_test "| Plugins | Plugin | README | \n | ------ | ------ | - | \n | Dropbox | [plugins/dropbox/README.md] | \n"
let () = do_test "1. one \n 2. two \n"
