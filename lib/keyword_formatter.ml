open Ast
open Output
open Config
open Print_helpers

(* Keyword formatting functionality *)
module KeywordFormatter (O : Output) (Config : sig val config : format_config end) = struct
  module PrintHelpers = PrintHelpers (O) (Config)
  open PrintHelpers
  open State.StateHelpers

  let format_with_conditional_newline_before state token indent_adjustment =
    if not (is_values_mode state) then (
      print_newline ();
      let adjusted_level = max 0 ((get_indent_level state) + indent_adjustment) in
      print_indented_token_with_space token adjusted_level
    ) else 
      print_token_with_space token

  let format_operator _state token =
    print_token_with_space token

  let format_create state before_token =
    match before_token with
    | None -> print_token_with_space CREATE
    | Some (COMMENT _) -> print_indented_token_with_space CREATE (get_indent_level state)
    | Some _ ->
        print_newline ();
        print_indented_token_with_space CREATE (get_indent_level state)

  let format_insert state before_token =
    match before_token with
    | None | Some (COMMENT _) -> print_indented_token_with_space INSERT (get_indent_level state)
    | Some BEGIN ->
        (* BEGIN already positioned us correctly with indentation, just print INSERT *)
        print_token_with_space INSERT
    | Some _ ->
        print_newline ();
        print_indented_token_with_space INSERT (get_indent_level state)

  let format_select state before_token next_token =
    match before_token, next_token with
    | _, Some LEFT_PAREN -> print_token_with_space SELECT
    | Some LEFT_PAREN, _ ->
        print_token_with_space SELECT;
        if not (is_values_mode state) then (
          print_newline ();
          increment_indent state;
          print_current_indent (get_indent_level state)
        )
    | _ ->
        print_indented_token_with_space SELECT (get_indent_level state);
        print_newline ();
        increment_indent state;
        print_current_indent (get_indent_level state)

  let format_values state =
    enable_values_mode state;
    increment_indent state;
    print_token_with_space VALUES

  let format_references state =
    enable_references_mode state;
    print_token_with_space REFERENCES

  let format_from state before_token =
    if not (is_values_mode state) then (
      decrement_indent state;
      (* Don't add extra newline if previous token was an inline comment *)
      (match before_token with
       | Some (INLINE_COMMENT _) -> ()
       | _ -> print_newline ());
      print_indented_token_with_space FROM (get_indent_level state)
    ) else (
      print_string " ";
      print_token_with_space FROM
    )

  let format_where state =
    if not (is_values_mode state) then (
      print_newline ();
      print_indented_token_with_space WHERE (get_indent_level state);
      print_newline ();
      increment_indent state;
      print_current_indent (get_indent_level state)
    ) else (
      print_string " ";
      print_token_with_space WHERE
    )

  let format_clause_keyword state token next_token =
    match token with
    | LEFT when next_token = Some JOIN ->
        (* LEFT JOIN should stay on same line *)
        disable_references_mode state;
        format_with_conditional_newline_before state token 0
    | JOIN when not (is_values_mode state) ->
        (* JOIN continues the LEFT JOIN on same line *)
        print_token_with_space token
    | AND | OR | RETURNS ->
        disable_references_mode state;
        format_with_conditional_newline_before state token 0
    | _ -> 
        print_token_with_space token

  let format_begin state =
    decrement_indent state;
    print_newline ();
    print_indented_token_with_space BEGIN (get_indent_level state);
    push_indent state;
    increment_indent state;
    print_newline ();
    print_current_indent (get_indent_level state)

  let format_end state =
    pop_indent state;
    print_newline ();
    print_indented_token END (get_indent_level state)

  let format_end_loop state =
    pop_indent state;
    print_indented_token ~extra_indent:1 END_LOOP (get_indent_level state)

  let format_loop state =
    print_newline ();
    pop_indent state;
    print_indented_token_with_space LOOP (get_indent_level state);
    print_newline ();
    increment_indent state;
    print_current_indent (get_indent_level state)

  let format_declare state =
    print_newline ();
    print_indented_token_with_space DECLARE (get_indent_level state);
    increment_indent state;
    print_newline ();
    print_current_indent (get_indent_level state)

  let format_in state =
    print_token_with_space IN;
    push_indent state;
    increment_indent state;
    print_newline ()
end
