open Pgcore.Config
open Pgcore.Output
open Parser

(* Formatter Builder Pattern for chaining configurations *)
module FormatterBuilder = struct
  type t = {
    config: Pgcore.Config.format_config;
    output: (module Pgcore.Output.Output);
  }
  
  let create () = { 
    config = Pgcore.Config.default_config; 
    output = (module Pgcore.Output.PrintOutput : Pgcore.Output.Output) 
  }
  
  let with_indent_size size builder = 
    { builder with config = { builder.config with indent_size = size }}
  
  let with_max_line_length length builder = 
    { builder with config = { builder.config with max_line_length = length }}
  
  let with_newline_after_semicolon flag builder = 
    { builder with config = { builder.config with newline_after_semicolon = flag }}
  
  let with_space_after_comma flag builder = 
    { builder with config = { builder.config with space_after_comma = flag }}
  
  let with_output output builder = { builder with output }
  
  let format_with_config builder lexbuf =
    let module Config = struct
      let config = builder.config
    end in
    let module Formatter = MakeFormatter ((val builder.output : Output)) (Config) in
    Formatter.format lexbuf
end
