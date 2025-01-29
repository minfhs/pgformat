open Pgformat
open Core

let format_content = function
  | None | Some "-" -> In_channel.input_all In_channel.stdin |> Pgformatter.format_string
  | Some file -> Pgformatter.format_file file
;;

let command =
  Command.basic
    ~summary:"File to format"
    ~readme:(fun () -> "Path to .sql file that you want to format with pgformat")
    (let%map_open.Command write = flag "-w" no_arg ~doc:" write to file"
     and filedesc = anon (maybe ("filename" %: string)) in
     fun () ->
       let data = format_content filedesc in
       if write then match filedesc with
         | Some file -> Out_channel.write_all file ~data
         | None -> Printf.eprintf "You can only write to file when a file is passed"
       else Printf.printf "%s" data)
;;

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
