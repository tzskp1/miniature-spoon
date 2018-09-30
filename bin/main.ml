open Core
open Miniature_spoon.Markdown
   
let command =
  Command.basic
    ~summary:"sing a song"
    (let open Command.Let_syntax in
     (* flags *)
     let%map_open input = flag "input" ~aliases:["-i"] (optional string) ~doc:"input file"
     and output = flag "output" ~aliases:["-o"] (optional string) ~doc:"output file"
     in
     fun () ->
     (* command body *)
     let src_ch =
       Option.map input ~f:(Fn.compose In_channel.create Filename.realpath)
       |> Option.value ~default:In_channel.stdin
     in
     let dst_ch =
       Option.map output ~f:Out_channel.create
       |> Option.value ~default:Out_channel.stdout
     in
     let () = 
       In_channel.input_all src_ch 
       |> parse
       |> extract
       |> Out_channel.output_string dst_ch
     in
     let () = In_channel.close src_ch
     in Out_channel.close dst_ch)

let () = Command.run command
