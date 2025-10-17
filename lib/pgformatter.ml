open Core
open Ast

(* Structured error types for better error handling *)
type parse_error = 
  | SyntaxError of string * Lexing.position
  | UnexpectedToken of string * Lexing.position
  | LexerError of string * Lexing.position

type 'a parse_result = ('a, parse_error) result

let string_of_parse_error = function
  | SyntaxError (msg, pos) -> 
      Printf.sprintf "Syntax error at line %d, column %d: %s" 
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg
  | UnexpectedToken (token, pos) -> 
      Printf.sprintf "Unexpected token '%s' at line %d, column %d" 
        token pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  | LexerError (msg, pos) -> 
      Printf.sprintf "Lexer error at line %d, column %d: %s" 
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg

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

(* Output interface for formatting *)
module type Output = sig
  val print_string : string -> unit
  val print_newline : unit -> unit
end

module PrintOutput : Output = struct
  let print_string s = printf "%s" s
  let print_newline () = printf "\n"
end

(* Types for formatter state *)
type formatter_state = {
  indent_level: int ref;
  indent_stack: int Stack.t ref;
  values_mode: bool ref;
  references_mode: bool ref;
}

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

(* State management functions *)
module StateHelpers = struct
  let get_indent_level state = !(state.indent_level)

  let set_indent_level state level = state.indent_level := level

  let increment_indent state = 
    state.indent_level := !(state.indent_level) + 1

  let decrement_indent state = 
    state.indent_level := max 0 (!(state.indent_level) - 1)

  let push_indent state =
    Stack.push !(state.indent_stack) !(state.indent_level)

  let pop_indent state =
    let level = Option.value ~default:0 (Stack.pop !(state.indent_stack)) in
    state.indent_level := level

  let enable_values_mode state = state.values_mode := true
  let disable_values_mode state = state.values_mode := false
  let is_values_mode state = !(state.values_mode)

  let enable_references_mode state = state.references_mode := true
  let disable_references_mode state = state.references_mode := false
  let is_references_mode state = !(state.references_mode)
end

(* Token classification *)
module TokenClassifier = struct
  let is_simple_operator = function
    | EQ | LT | GT | GTE | LTE | NEQ | INTO | ASSIGN | LANGUAGE
    | INT _ | FLOAT _ | FUNC_DELIM | AS | PLUS | MINUS | STAR
    | PARAMETER _ | JSON_OP _ | TEXT_SEARCH_OP _ -> true
    | _ -> false

  let is_structural_keyword = function
    | SELECT | FROM | WHERE | CREATE | INSERT | VALUES | BEGIN | END
    | DECLARE | LOOP | END_LOOP -> true
    | _ -> false

  let is_join_keyword = function
    | LEFT | RIGHT | INNER | OUTER | FULL | JOIN | ON -> true
    | _ -> false

  let is_clause_keyword = function
    | AND | OR | NOT | RETURNS | COMMA -> true
    | _ -> false

  let requires_space_after = function
    | SEMICOLON -> false
    | _ -> true

  let requires_newline_before = function
    | SELECT | FROM | WHERE | CREATE | INSERT | BEGIN | END 
    | DECLARE | AND | OR | LEFT | JOIN -> true
    | _ -> false
end

(* Core formatting logic for different token types *)
module TokenFormatters (O : Output) (Config : sig val config : format_config end) = struct
  module PrintHelpers = PrintHelpers (O) (Config)
  open PrintHelpers
  open StateHelpers

  (* Common helper functions to reduce duplication *)
  let format_with_conditional_space state token next_token =
    let space = 
      if Poly.(next_token = Some SEMICOLON) || is_values_mode state 
      then "" 
      else " " 
    in
    print_string ((string_of_token token) ^ space)

  let format_with_conditional_newline_before state token indent_adjustment =
    if not (is_values_mode state) then (
      print_newline ();
      let adjusted_level = max 0 ((get_indent_level state) + indent_adjustment) in
      print_indented_token_with_space token adjusted_level
    ) else 
      print_token_with_space token

  let format_structural_keyword_with_indent state token =
    print_indented_token_with_space token (get_indent_level state);
    print_newline ();
    increment_indent state;
    print_current_indent (get_indent_level state)

  let format_comment _state comment next_token =
    match next_token with
    | Some SELECT | Some INSERT -> print_string ("/*" ^ comment ^ "*/"); print_newline ()
    | _ -> print_string ("/*" ^ comment ^ "*/")

  let format_inline_comment _state _comment =
    print_token _comment;
    print_newline ()

  let format_simple_token _state token =
    print_token token

  let format_string_literal _state token =
    print_token token

  let format_operator _state token =
    print_token_with_space token

  let format_array _state array_content =
    print_string array_content

  let format_parameter _state param =
    print_string param

  let format_special_operator _state op =
    print_string op

  let format_identifier state id next_token =
    format_with_conditional_space state (ID id) next_token

  let format_null state next_token =
    format_with_conditional_space state NULL next_token

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
    | LEFT when Poly.(next_token = Some JOIN) ->
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
    if not (Poly.(next_token = Some SEMICOLON)) then print_string " "

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

  let format_func_delim before_token =
    match before_token with
    | Some AS -> 
        print_newline ();
        print_token FUNC_DELIM
    | Some LANGUAGE ->
        print_string " ";
        print_token FUNC_DELIM
    | _ -> 
        print_token FUNC_DELIM
end

(* Create initial formatter state *)
let create_formatter_state () = {
  indent_level = ref 0;
  indent_stack = ref (Stack.create ());
  values_mode = ref false;
  references_mode = ref false;
}

(* Main token formatting dispatch *)
module MakeFormatter (O : Output) (Config : sig val config : format_config end) = struct
  module TokenFormatters = TokenFormatters (O) (Config)
  module PrintHelpers = PrintHelpers (O) (Config)
  
  let format_token state token before_token after_token =
    let open TokenFormatters in
    
    let format_comment_tokens state token _before_token after_token =
      match token with
      | COMMENT c -> format_comment state c after_token
      | INLINE_COMMENT _ -> format_inline_comment state token
      | _ -> failwith "Expected comment token"
    in
    
    let format_literal_tokens state token _before_token after_token =
      match token with
      | SSTRING _ | DSTRING _ | QUOTED_ID _ -> format_string_literal state token
      | COLONS -> format_simple_token state token
      | ID id -> format_identifier state id after_token
      | NULL -> format_null state after_token
      | ARRAY a -> format_array state a
      | PARAMETER p -> format_parameter state p
      | JSON_OP op | TEXT_SEARCH_OP op -> format_special_operator state op
      | _ -> failwith "Expected literal token"
    in
    
    let format_structural_keywords state token before_token after_token =
      match before_token, token, after_token with
      | before, CREATE, _ -> format_create state before
      | before, INSERT, _ -> format_insert state before
      | before, SELECT, next -> format_select state before next
      | _, VALUES, _ -> format_values state
      | _, REFERENCES, _ -> format_references state
      | before, FROM, _ -> format_from state before
      | _, WHERE, _ -> format_where state
      | _, BEGIN, _ -> format_begin state
      | _, END, _ -> format_end state
      | _, END_LOOP, _ -> format_end_loop state
      | _, LOOP, _ -> format_loop state
      | _, DECLARE, _ -> format_declare state
      | _, IN, _ -> format_in state
      | _ -> failwith "Expected structural keyword"
    in
    
    let format_punctuation state token before_token after_token =
      match before_token, token, after_token with
      | before, COMMA, next -> format_comma state before next
      | _, LEFT_PAREN, Some RIGHT_PAREN -> format_simple_token state token
      | Some LEFT_PAREN, RIGHT_PAREN, _ -> 
          format_simple_token state token; PrintHelpers.print_string " "
      | _, LEFT_PAREN, next -> format_left_paren state next
      | before, RIGHT_PAREN, next -> format_right_paren state before next
      | _, SEMICOLON, next -> format_semicolon state next
      | before, FUNC_DELIM, _ -> format_func_delim before
      | _ -> failwith "Expected punctuation token"
    in

    match token with
    (* Comments *)
    | COMMENT _ | INLINE_COMMENT _ -> format_comment_tokens state token before_token after_token
    
    (* String literals and identifiers *)
    | SSTRING _ | DSTRING _ | QUOTED_ID _ | COLONS | ID _ | NULL | ARRAY _ | PARAMETER _ | JSON_OP _ | TEXT_SEARCH_OP _ ->
        format_literal_tokens state token before_token after_token
    
    (* Simple operators *)
    | tok when TokenClassifier.is_simple_operator tok -> format_operator state tok
    
    (* Structural keywords *)
    | CREATE | INSERT | SELECT | VALUES | REFERENCES | FROM | WHERE | BEGIN | END | END_LOOP | LOOP | DECLARE | IN ->
        format_structural_keywords state token before_token after_token
    
    (* Clause keywords *)
    | AND | OR | LEFT | RIGHT | JOIN | RETURNS -> 
        format_clause_keyword state token after_token
    
    (* Inline keywords that should just have spaces *)
    | ON | NOT | IS | TABLE | IF | EXISTS | PRIMARY | KEY | INDEX | UNIQUE | DEFAULT ->
        PrintHelpers.print_token_with_space token
    
    (* Punctuation *)
    | COMMA | LEFT_PAREN | RIGHT_PAREN | SEMICOLON | FUNC_DELIM ->
        format_punctuation state token before_token after_token
    
    (* Default case - just print with space *)
    | _ -> PrintHelpers.print_token_with_space token

  (* Parse error handling with structured error types *)
  let parse_with_error lexbuf =
    try Ok (Lexer.read lexbuf) with
    | SyntaxError msg ->
        let pos = lexbuf.lex_curr_p in
        Error (SyntaxError (msg, pos))
    | Failure msg when String.is_prefix msg ~prefix:"lexing" ->
        let pos = lexbuf.lex_curr_p in
        Error (LexerError (msg, pos))
    | exn ->
        let pos = lexbuf.lex_curr_p in
        Error (SyntaxError (Exn.to_string exn, pos))

  (* Token history management *)
  let take2 = function
    | a :: b :: _ -> [a; b]
    | lst -> lst

  (* Main parsing loop with error handling *)
  let rec parse state lexbuf tokens =
    match parse_with_error lexbuf with
    | Error error ->
        Printf.eprintf "%s\n" (string_of_parse_error error)
    | Ok next_token ->
        (* Format current token based on context *)
        (match tokens with
         | current :: previous :: _ ->
           format_token state current (Some previous) (Some next_token)
         | current :: _ ->
           format_token state current None (Some next_token)
         | _ -> ());
        (* Continue parsing *)
        match next_token with
        | EOF ->
           format_token state EOF None None
        | token ->
           parse state lexbuf (take2 (token :: tokens))

  (* Main format function *)
  let format lexbuf =
    let state = create_formatter_state () in
    Stack.push !(state.indent_stack) !(state.indent_level);
    parse state lexbuf []
end

(* Default formatter using PrintOutput *)
module DefaultConfig = struct
  let config = default_config
end

module DefaultFormatter = MakeFormatter (PrintOutput) (DefaultConfig)

(* Parse error handling *)
let parse_with_error = DefaultFormatter.parse_with_error

(* Token history management *)
let take2 = DefaultFormatter.take2

(* Main parsing loop *)
let parse = DefaultFormatter.parse

(* Main format function *)
let format = DefaultFormatter.format

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
