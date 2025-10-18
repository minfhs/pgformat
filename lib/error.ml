open Core

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
