open Pgformat
open Core

let format_content ~indent_size ~max_line_length = function
  | None | Some "-" -> 
      (* Format from stdin *)
      let lexbuf = Lexing.from_channel In_channel.stdin in
      let builder = Builder.FormatterBuilder.create ()
        |> Builder.FormatterBuilder.with_indent_size indent_size
        |> Builder.FormatterBuilder.with_max_line_length max_line_length in
      Builder.FormatterBuilder.format_with_config builder lexbuf
  | Some file -> 
      (* Format from file *)
      In_channel.with_file file ~f:(fun ic ->
        let lexbuf = Lexing.from_channel ic in
        let builder = Builder.FormatterBuilder.create ()
          |> Builder.FormatterBuilder.with_indent_size indent_size
          |> Builder.FormatterBuilder.with_max_line_length max_line_length in
        Builder.FormatterBuilder.format_with_config builder lexbuf)
;;

let command =
  Command.basic
    ~summary:"Format SQL files with configurable options"
    ~readme:(fun () -> "Path to .sql file that you want to format with pgformat. Use '-' or omit for stdin.")
    (let%map_open.Command 
       filedesc = anon (maybe ("filename" %: string))
     and indent_size = flag "-indent-size" (optional_with_default 4 int) 
         ~doc:"N Number of spaces for indentation (default: 4)"
     and max_line_length = flag "-max-line-length" (optional_with_default 120 int)
         ~doc:"N Maximum line length before wrapping (default: 120)"
     in
     fun () -> format_content ~indent_size ~max_line_length filedesc)
;;

let () = Command_unix.run ~version:"2.0" ~build_info:"pgformat with FormatterBuilder API" command
