open Core

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

(* Parse a single configuration line *)
let parse_config_line line =
  let line = String.strip line in
  if String.is_empty line || String.is_prefix line ~prefix:"#" then
    None
  else
    match String.split line ~on:'=' with
    | [key; value] ->
        let key = String.strip key in
        let value = String.strip value in
        Some (key, value)
    | _ -> None

(* Apply a configuration setting *)
let apply_config_setting config key value =
  match key with
  | "indent_size" | "indent-size" ->
      (match Int.of_string value with
       | size when size > 0 -> { config with indent_size = size }
       | _ -> config)
  | "max_line_length" | "max-line-length" ->
      (match Int.of_string value with
       | length when length > 0 -> { config with max_line_length = length }
       | _ -> config)
  | "newline_after_semicolon" | "newline-after-semicolon" ->
      (match String.lowercase value with
       | "true" | "yes" | "1" -> { config with newline_after_semicolon = true }
       | "false" | "no" | "0" -> { config with newline_after_semicolon = false }
       | _ -> config)
  | "space_after_comma" | "space-after-comma" ->
      (match String.lowercase value with
       | "true" | "yes" | "1" -> { config with space_after_comma = true }
       | "false" | "no" | "0" -> { config with space_after_comma = false }
       | _ -> config)
  | _ -> config

(* Load configuration from a file *)
let load_config_from_file filename =
  try
    let lines = In_channel.read_lines filename in
    List.fold lines ~init:default_config ~f:(fun config line ->
      match parse_config_line line with
      | Some (key, value) -> apply_config_setting config key value
      | None -> config)
  with
  | _ -> default_config

(* Load configuration from .pgformat file in current directory *)
let load_config_from_pwd () =
  let config_file = ".pgformat" in
  if Stdlib.Sys.file_exists config_file then
    load_config_from_file config_file
  else
    default_config

(* Merge command-line options with config file *)
let merge_config_with_overrides config ~indent_size ~max_line_length =
  { config with
    indent_size = (match indent_size with Some s -> s | None -> config.indent_size);
    max_line_length = (match max_line_length with Some l -> l | None -> config.max_line_length);
  }
