{
open Lexing

exception SyntaxError of string

type t =
	| INT of int
	| FLOAT of float
	| TRUE
	| FALSE
	| NULL
	| LEFT_PAREN
	| RIGHT_PAREN
	| LEFT_BRACE
	| RIGHT_BRACE
	| LEFT_BRACK
	| RIGHT_BRACK
	| COLON
	| SEMICOLON
	| COMMA
	| EOF
	| STRING of string
    | ID of string
    | SELECT
    | AS
    | FROM
    | INTO
    | VALUES
    | WHERE
    | INSERT
    | FUNC_DELIM
    | ASSIGN
    | EQ
    | PLUS
    | MINUS
    | TODO
	
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let int = '-'? ['0'-'9'] ['0'-'9']*
let float = digit* frac? exp?

let id = ['a'-'z' 'A'-'Z' '_' '.']+

let select="SELECT" | "Select" | "select"
let askw = "AS" | "As" | "as"
let where = "WHERE" | "Where" | "where"
let into = "INTO" | "Into" | "into"
let values = "VALUES" | "Values" | "values"
let from = "FROM" | "From" | "from"
let insert = "INSERT" | "Insert" | "insert"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "false"  { FALSE }
  | "null"   { NULL }
  | '\''      { read_string (Buffer.create 17) lexbuf }
  | "$$"     { FUNC_DELIM }
  | '('      { LEFT_PAREN}
  | ')'      { RIGHT_PAREN}
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | ':'      { COLON }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | ":=" { ASSIGN }
  | "=" { EQ }
  | select { SELECT }
  | from { FROM }
  | insert { INSERT }
  | where { WHERE }
  | into { INTO }
  | values { VALUES }
  | askw { AS }
  | id { ID(Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
  | '\''       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
