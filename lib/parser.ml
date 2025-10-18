open Core
open Pgcore.Ast
open Pgcore.State

(* Main token formatting dispatch *)
module MakeFormatter (O : Pgcore.Output.Output) (Config : sig val config : Pgcore.Config.format_config end) = struct
  (* Import the formatter modules *)
  module CommentFormatter = Formatters.Comment_formatter.CommentFormatter (O) (Config)
  module KeywordFormatter = Formatters.Keyword_formatter.KeywordFormatter (O) (Config)
  module LiteralFormatter = Formatters.Literal_formatter.LiteralFormatter (O) (Config)
  module PunctuationFormatter = Formatters.Punctuation_formatter.PunctuationFormatter (O) (Config)

  (* Helper functions *)
  let print_string s = O.print_string s

  let format_token state token before_token after_token =
    match token with
    (* Comments - handle specially *)
    | COMMENT c -> 
        CommentFormatter.format_comment state c after_token
    | INLINE_COMMENT _ -> 
        CommentFormatter.format_inline_comment state token
    
    (* Specific structural keywords that need special handling *)
    | CREATE ->
        KeywordFormatter.format_create state before_token
    | INSERT ->
        KeywordFormatter.format_insert state before_token
    | SELECT when (Poly.(before_token = Some FUNC_DELIM)) ->
        (* Add newline before SELECT when it follows $$ in function body *)
        O.print_newline ();
        KeywordFormatter.format_select state before_token after_token
    | SELECT ->
        KeywordFormatter.format_select state before_token after_token
    | VALUES ->
        KeywordFormatter.format_values state
    | REFERENCES ->
        KeywordFormatter.format_references state
    | FROM ->
        KeywordFormatter.format_from state before_token
    | WHERE ->
        KeywordFormatter.format_where state
    | BEGIN ->
        KeywordFormatter.format_begin state
    | END ->
        KeywordFormatter.format_end state
    | END_LOOP ->
        KeywordFormatter.format_end_loop state
    | LOOP ->
        KeywordFormatter.format_loop state
    | DECLARE ->
        KeywordFormatter.format_declare state
    | IN ->
        KeywordFormatter.format_in state
    
    (* JOIN-related keywords using classifier *)
    | LEFT when (Poly.(after_token = Some JOIN)) ->
        KeywordFormatter.format_with_conditional_newline_before state token 0
    | token when Pgcore.Token_classifier.TokenClassifier.is_join_keyword token ->
        KeywordFormatter.format_operator state token
    
    (* Clause keywords using classifier *)
    | token when Pgcore.Token_classifier.TokenClassifier.is_clause_keyword token -> 
        KeywordFormatter.format_with_conditional_newline_before state token 0
    
    (* Punctuation *)
    | LEFT_PAREN ->
        PunctuationFormatter.format_left_paren state after_token
    | RIGHT_PAREN ->
        PunctuationFormatter.format_right_paren state before_token after_token
    | COMMA ->
        PunctuationFormatter.format_comma state before_token after_token
    | SEMICOLON ->
        PunctuationFormatter.format_semicolon state after_token
    | FUNC_DELIM ->
        PunctuationFormatter.format_func_delim before_token
    
    (* String literals and identifiers using the appropriate functions *)
    | SSTRING _ | DSTRING _ ->
        LiteralFormatter.format_string_literal state token
    | QUOTED_ID _ ->
        LiteralFormatter.format_simple_token state token
    | ID s ->
        LiteralFormatter.format_identifier state s after_token
    | NULL ->
        LiteralFormatter.format_null state after_token
    | ARRAY a ->
        LiteralFormatter.format_array state a
    | PARAMETER p ->
        LiteralFormatter.format_parameter state p
    | JSON_OP op | TEXT_SEARCH_OP op ->
        LiteralFormatter.format_special_operator state op
    
    (* Simple operators using classifier *)
    | INT _ | FLOAT _ ->
        (* Handle numbers like identifiers - only add space when needed *)
        let space = if Poly.(after_token = Some SEMICOLON) then "" else " " in
        print_string ((string_of_token token) ^ space)
    | STAR ->
        (* Handle STAR specially - don't add space when followed by RIGHT_PAREN *)
        let space = if Poly.(after_token = Some RIGHT_PAREN) then "" else " " in
        print_string ((string_of_token token) ^ space)
    | LANGUAGE when (Poly.(before_token = Some FUNC_DELIM)) ->
        (* Add space before LANGUAGE when it follows $$ *)
        print_string " ";
        KeywordFormatter.format_operator state token
    | token when Pgcore.Token_classifier.TokenClassifier.is_simple_operator token -> 
        KeywordFormatter.format_operator state token
    
    (* Structural keywords using classifier *)
    | token when Pgcore.Token_classifier.TokenClassifier.is_structural_keyword token ->
        KeywordFormatter.format_with_conditional_newline_before state token 0
    
    (* Other keywords that just need operator formatting *)
    | ON | NOT | IS | TABLE | IF | EXISTS | PRIMARY | KEY | INDEX | UNIQUE | DEFAULT | RIGHT ->
        KeywordFormatter.format_operator state token
    
    (* Default case *)
    | _ -> 
        KeywordFormatter.format_operator state token

  (* Parse error handling *)
  let parse_with_error lexbuf =
    try Ok (Pgcore.Lexer.read lexbuf) with
    | Pgcore.Ast.SyntaxError msg ->
        Error msg
    | Failure msg when String.is_prefix msg ~prefix:"lexing" ->
        Error msg
    | exn ->
        Error (Exn.to_string exn)

  (* Token history management *)
  let take2 = function
    | a :: b :: _ -> [a; b]
    | lst -> lst

  (* Main parsing loop with error handling *)
  let rec parse state lexbuf tokens =
    match parse_with_error lexbuf with
    | Error error ->
        Printf.eprintf "%s\n" error
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
    let state = Pgcore.State.create_formatter_state () in
    Stack.push !(state.indent_stack) !(state.indent_level);
    parse state lexbuf []
end
