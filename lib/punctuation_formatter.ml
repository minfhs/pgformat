open Ast
open Output
open Config
open Print_helpers

(* Punctuation formatting functionality *)
module PunctuationFormatter (O : Output) (Config : sig val config : format_config end) = struct
  module PrintHelpers = PrintHelpers (O) (Config)
  open PrintHelpers
  open State.StateHelpers

  let format_left_paren state next_token =
    print_token LEFT_PAREN;
    match next_token with
    | Some SELECT ->
        if not (is_values_mode state) then (
          push_indent state;
          increment_indent state;
          print_newline ();
          print_current_indent (get_indent_level state)
        )
    | Some RIGHT_PAREN ->
        (* Empty parentheses - keep on same line *)
        ()
    | Some STAR ->
        (* Simple cases like COUNT(star) - keep on same line *)
        ()
    | _ ->
        if is_references_mode state then
          print_string " "
        else if (not (is_values_mode state)) && not (is_references_mode state) then (
          push_indent state;
          increment_indent state;
          print_newline ();
          print_current_indent (get_indent_level state)
        )

  let format_right_paren state before_token next_token =
    (* Special handling for simple parenthetical expressions *)
    match before_token with
    | Some LEFT_PAREN ->
        (* Empty parentheses () - use simple formatting *)
        print_token RIGHT_PAREN;
        if not (next_token = Some SEMICOLON) then print_string " "
    | Some STAR ->
        (* Simple cases like COUNT(star) - keep inline *)
        print_token RIGHT_PAREN;
        if not (next_token = Some SEMICOLON) then print_string " "
    | _ ->
        if (not (is_values_mode state)) && not (is_references_mode state) then (
          (* Don't add extra newline if previous token was an inline comment *)
          (match before_token with
           | Some (INLINE_COMMENT _) -> ()
           | _ -> print_newline ());
          pop_indent state;
          print_indented_token RIGHT_PAREN (get_indent_level state)
        ) else 
          print_token RIGHT_PAREN;
        disable_references_mode state;
        if not (next_token = Some SEMICOLON) then print_string " "

  let format_semicolon state next_token =
    (* Handle values mode cleanup *)
    if is_values_mode state then (
      decrement_indent state;
      disable_values_mode state
    );
    print_token SEMICOLON;
    match next_token with
    | Some EOF | None -> 
        print_newline ()
    | Some SELECT when (get_indent_level state) <= 1 ->
        (* Reset indentation for new SELECT statement only at top level *)
        set_indent_level state 0;
        print_newline ();
        print_newline ()
    | Some (CREATE | INSERT) when (get_indent_level state) <= 1 -> 
        (* Reset indentation for new top-level statement, let the handler add its newline *)
        set_indent_level state 0;
        print_newline ()
    | Some LANGUAGE ->
        (* Don't add extra newline after semicolon before LANGUAGE *)
        print_string " "
    | Some BEGIN ->
        (* After variable declarations, decrease indent before BEGIN *)
        (* Don't add newline - BEGIN handler will add its own *)
        decrement_indent state
    | Some (ID _) ->
        (* Variable declaration continues, maintain indentation *)
        print_newline ();
        print_current_indent (get_indent_level state)
    | Some END ->
        (* Don't add newline - END handler will add its own *)
        ()
    | _ -> 
        print_newline ()

  let format_comma state before_token next_token =
    match before_token, next_token with
    | Some RIGHT_PAREN, Some LEFT_PAREN when is_values_mode state ->
        print_newline ();
        print_indented_token_with_space COMMA (get_indent_level state)
    | Some (INLINE_COMMENT _), _ when not (is_values_mode state) ->
        (* Don't add extra newline after inline comment *)
        print_indented_token_with_space COMMA (get_indent_level state)
    | _ ->
        if not (is_values_mode state) then (
          print_newline ();
          print_indented_token_with_space COMMA (get_indent_level state)
        ) else 
          print_token_with_space COMMA

  let format_func_delim before_token =
    match before_token with
    | Some AS -> 
        print_token FUNC_DELIM
    | Some LANGUAGE ->
        print_string " ";
        print_token FUNC_DELIM
    | _ -> 
        print_token FUNC_DELIM
end
