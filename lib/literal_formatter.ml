open Ast
open Output
open Config
open Print_helpers

(* Literal formatting functionality *)
module LiteralFormatter (O : Output) (Config : sig val config : format_config end) = struct
  module PrintHelpers = PrintHelpers (O) (Config)
  open PrintHelpers
  open State.StateHelpers

  (* Common helper functions to reduce duplication *)
  let format_with_conditional_space state token next_token =
    let space = 
      if next_token = Some SEMICOLON || is_values_mode state 
      then "" 
      else " " 
    in
    print_string ((string_of_token token) ^ space)

  let format_simple_token _state token =
    print_token token

  let format_string_literal _state token =
    print_token token

  let format_array _state array_content =
    print_string array_content

  let format_parameter _state param =
    print_string param

  let format_special_operator _state op =
    print_string op

  let format_identifier state id next_token =
    (* Function calls should not have space before parenthesis *)
    let space = 
      if next_token = Some SEMICOLON || next_token = Some LEFT_PAREN || is_values_mode state 
      then "" 
      else " " 
    in
    print_string ((string_of_token (ID id)) ^ space)

  let format_null state next_token =
    format_with_conditional_space state NULL next_token
end
