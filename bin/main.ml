open Core
   
let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)
