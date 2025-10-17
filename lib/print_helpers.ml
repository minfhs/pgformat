open Ast
open Output
open Config

(* Helper functions for printing and indentation *)
module PrintHelpers (O : Output) (Config : sig val config : format_config end) = struct
  let indent_size = Config.config.indent_size

  let make_indent level = String.make (indent_size * level) ' '

  let print_newline () = O.print_newline ()

  let print_token token = O.print_string (string_of_token token)

  let print_token_with_space token = O.print_string ((string_of_token token) ^ " ")

  let print_indented_token ?(extra_indent = 0) token level =
    O.print_string (make_indent (level + extra_indent) ^ (string_of_token token))

  let print_indented_token_with_space ?(extra_indent = 0) token level =
    O.print_string (make_indent (level + extra_indent) ^ (string_of_token token) ^ " ")

  let print_newline_token token =
    O.print_newline ();
    O.print_string (string_of_token token)

  let print_current_indent level = O.print_string (make_indent level)
  
  let print_string s = O.print_string s
end
