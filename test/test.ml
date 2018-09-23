module M = Markdown_parser.Markdown
module P = Markdown_parser.Parser 
open Base
   
let test_link1 = "[an example][id]\n \n"
let test_atom1 = "This is [an example] [id] reference-style link.\n \n"
let test_atom2 = "This is a content.\n \n"
let test_atom3 = "### This is a title.\n \n"
let test_list1 = "1. one \n 2. two \n \n"
let test_paragraph1 = "## This is a header.\n This is a content.\n \n"
let test_paragraph2 = "## This is a header.\n ### This is a sub header. ### \n nth23 \n"
let test_paragraph2' = "## This is a header.\n ### ### This is a sub header. ### \n nth23 \n"
let test_paragraph3 = "### This is a sub header. ### \n nth23 \n"

let do_test data =
  let res, rest = P.run M.paragraph data in
  begin match res with
  | First r ->
     M.string_of_paragraph r
     |> ((^) "Success: ")
  | _ -> "Fail: " ^ rest
  end |> Stdio.print_endline

let () = do_test test_paragraph1
let () = do_test test_atom2
let () = do_test test_atom3
let () = do_test test_list1
let () = do_test test_paragraph2
let () = do_test test_paragraph2'
let () = do_test test_paragraph3
