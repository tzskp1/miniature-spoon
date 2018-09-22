module M = Markdown
open Parser
   
let test_link1 = "[an example][id]"
let test_link2 = "This is [an example] [id] reference-style link."
    
let res, rest = run M.link test_link1
let () =
  match res with
  | First r ->
     M.string_of_atom r
     |> Stdio.print_endline
  | _ -> ()
