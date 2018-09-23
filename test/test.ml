module M = Markdown_parser.Markdown
module P = Markdown_parser.Parser 
open Base
   
let test_link1 = "[an example][id]\n"
let test_atom1 = "This is [an example] [id] reference-style link.\n"
let test_atom2 = "This is a content.\n"
let test_list1 = "1. one \n 2. two \n"
let test_paragraph1 = "## This is a header.\n This is a content.\n"
let testdata = [ test_link1 ; test_atom1 ; test_paragraph1 ]

let do_test_par data =
  let res, rest = P.run M.paragraph data in
  begin match res with
  | First r ->
     M.string_of_paragraph r
     |> ((^) "Success: ")
  | _ -> "Fail: " ^ rest
  end |> Stdio.print_endline
           
let do_test_atom data =
  let res, rest = P.run M.lines data in
  begin match res with
  | First r ->
     List.map ~f:M.string_of_atom r
     |> List.fold_left ~init:"Success: " ~f:(^)
  | _ -> "Fail: " ^ rest
  end |> Stdio.print_endline

let () = do_test_par test_paragraph1
let () = do_test_atom test_atom2
let () = do_test_par test_atom2
let () = do_test_par test_list1
