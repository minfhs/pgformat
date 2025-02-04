open Pgformat
open Core

(* let () = Pgformatter.format_file "./fixture/complex.sql" *)

let format_content = function
  | None | Some "-" -> Pgformatter.format_stdio ()
  | Some file -> Pgformatter.format_file file
;;

let command =
  Command.basic
    ~summary:"File to format"
    ~readme:(fun () -> "Path to .sql file that you want to format with pgformat")
    (let%map_open.Command filedesc = anon (maybe ("filename" %: string)) in
     fun () -> format_content filedesc)
;;

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
