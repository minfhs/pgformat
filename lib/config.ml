(* Configuration for formatting rules *)
type format_config = {
  indent_size: int;
  max_line_length: int;
  newline_after_semicolon: bool;
  space_after_comma: bool;
}

let default_config = {
  indent_size = 4;
  max_line_length = 120;
  newline_after_semicolon = true;
  space_after_comma = true;
}
