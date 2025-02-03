{
open Lexing

open Ast
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let int = '-'? ['0'-'9'] ['0'-'9']*
let float = '-'? digit* frac? exp?

let idparts = ['a'-'z' 'A'-'Z' '_' '.' '$']+
let id = idparts (digit? idparts?)+

let begin="BEGIN"|"Begin"|"begin"
let select="SELECT" | "Select" | "select"
let askw = "AS" | "As" | "as"
let where = "WHERE" | "Where" | "where"
let into = "INTO" | "Into" | "into"
let values = "VALUES" | "Values" | "values"
let from = "FROM" | "From" | "from"
let insert = "INSERT" | "Insert" | "insert"
let create = "CREATE" | "Create" | "create"
let left = "LEFT" | "Left" | "left"
let andkw = "AND" | "And" | "and"
let returns = "RETURNS" | "Returns" | "returns"
let declare = "DECLARE" | "Declare" | "declare"
let in = "IN" | "In" | "in"
let language = "LANGUAGE" | "Language" | "language"
let loop = "LOOP" | "Loop" | "loop"
let end = "END" | "End" | "end"
let end_loop = end white+ loop 
let func_delim = ['$'] id* ['$']
let array_lit = ['['] [^ ']']* [']']
let colons = "::"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | "/*"     { read_comment (Buffer.create 1024) lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "null"   { NULL }
  | '\''      { read_sstring (Buffer.create 256) lexbuf }
  | '"'      { read_dstring (Buffer.create 256) lexbuf }
  | func_delim     { FUNC_DELIM }
  | '('      { LEFT_PAREN}
  | ')'      { RIGHT_PAREN}
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | colons { COLONS }
  | ':'      { COLON }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | ":=" { ASSIGN }
  | "=" { EQ }
  | begin { BEGIN }
  | select { SELECT }
  | from { FROM }
  | insert { INSERT }
  | where { WHERE }
  | into { INTO }
  | values { VALUES }
  | askw { AS }
  | andkw { AND }
  | create { CREATE }
  | left { LEFT }
  | returns { RETURNS }
  | declare { DECLARE }
  | in { IN }
  | language { LANGUAGE }
  | end_loop { END_LOOP }
  | loop { LOOP }
  | array_lit { ARRAY(Lexing.lexeme lexbuf) }
  | id { ID(Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_sstring buf =
  parse
  | '\\' '/'  { Buffer.add_char buf '/'; read_sstring buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_sstring buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_sstring buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_sstring buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_sstring buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_sstring buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_sstring buf lexbuf }
  | "\\'"  { Buffer.add_string buf "\\'"; read_sstring buf lexbuf }
  | [^ '\'']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_sstring buf lexbuf
    }
  | '\''       { SSTRING (Buffer.contents buf) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_dstring buf =
  parse
  | '\\' '/'  { Buffer.add_char buf '/'; read_dstring buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_dstring buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_dstring buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_dstring buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_dstring buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_dstring buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_dstring buf lexbuf }
  | "\\\""  { Buffer.add_string buf "\\\""; read_dstring buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_dstring buf lexbuf
    }
  | '"'       { DSTRING (Buffer.contents buf) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment buf =
  parse
  | "*/"     { COMMENT (Buffer.contents buf) }
  | _
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_comment buf lexbuf
    }
  | eof { raise (SyntaxError ("String is not terminated")) }
