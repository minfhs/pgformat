open Core

(* Module aliases for easier access *)
module Config = Pgcore.Config
module Builder = Pgformat.Builder

let format_content ~indent_size_override ~max_line_length_override = function
  | None | Some "-" -> 
      (* Format from stdin *)
      let lexbuf = Lexing.from_channel In_channel.stdin in
      let base_config = Config.load_config_from_pwd () in
      let config = Config.merge_config_with_overrides base_config 
        ~indent_size:indent_size_override ~max_line_length:max_line_length_override in
      let builder = Builder.FormatterBuilder.create ()
        |> Builder.FormatterBuilder.with_indent_size config.Config.indent_size
        |> Builder.FormatterBuilder.with_max_line_length config.Config.max_line_length in
      Builder.FormatterBuilder.format_with_config builder lexbuf
  | Some file -> 
      (* Format from file *)
      In_channel.with_file file ~f:(fun ic ->
        let lexbuf = Lexing.from_channel ic in
        let base_config = Config.load_config_from_pwd () in
        let config = Config.merge_config_with_overrides base_config 
          ~indent_size:indent_size_override ~max_line_length:max_line_length_override in
        let builder = Builder.FormatterBuilder.create ()
          |> Builder.FormatterBuilder.with_indent_size config.Config.indent_size
          |> Builder.FormatterBuilder.with_max_line_length config.Config.max_line_length in
        Builder.FormatterBuilder.format_with_config builder lexbuf)
;;

let command =
  Command.basic
    ~summary:"Format SQL files with configurable options"
    ~readme:(fun () -> "Path to .sql file that you want to format with pgformat. Use '-' or omit for stdin. Configuration can be loaded from .pgformat file in current directory.")
    (let%map_open.Command 
       filedesc = anon (maybe ("filename" %: string))
     and indent_size = flag "-indent-size" (optional int) 
         ~doc:"N Number of spaces for indentation (overrides .pgformat file)"
     and max_line_length = flag "-max-line-length" (optional int)
         ~doc:"N Maximum line length before wrapping (overrides .pgformat file)"
     in
     fun () -> format_content ~indent_size_override:indent_size ~max_line_length_override:max_line_length filedesc)
;;

let () = Command_unix.run ~version:"2.0" ~build_info:"pgformat with FormatterBuilder API" command
