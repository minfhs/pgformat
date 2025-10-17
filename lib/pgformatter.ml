open Core

(* Default formatter using PrintOutput *)
module DefaultConfig = struct
  let config = Config.default_config
end

module DefaultFormatter = Parser.MakeFormatter (Output.PrintOutput) (DefaultConfig)

(* Main API functions *)
let format = DefaultFormatter.format
let parse_with_error = DefaultFormatter.parse_with_error
let take2 = DefaultFormatter.take2
let parse = DefaultFormatter.parse

(* Public API functions *)
let format_stdio () =
  let input = In_channel.stdin in
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "STDIN" };
  format lexbuf;
  In_channel.close input

let format_file filename =
  let input = In_channel.create filename in
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  format lexbuf;
  In_channel.close input

(* Re-export modules for convenience *)
module Config = Config
module Output = Output
module Error = Error
module Builder = Builder
