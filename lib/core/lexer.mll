{
open Lexing
open Ast

(* Keyword lookup function for case-insensitive matching *)
let keyword_table = Hashtbl.create 50

let () =
  List.iter (fun (kw, token) -> 
    Hashtbl.add keyword_table (String.uppercase_ascii kw) token;
    Hashtbl.add keyword_table (String.lowercase_ascii kw) token;
    Hashtbl.add keyword_table kw token)
  [
    ("BEGIN", BEGIN); ("SELECT", SELECT); ("AS", AS); ("WHERE", WHERE);
    ("INTO", INTO); ("VALUES", VALUES); ("FROM", FROM); ("INSERT", INSERT);
    ("CREATE", CREATE); ("LEFT", LEFT); ("AND", AND); ("OR", OR); ("NOT", NOT);
    ("RETURNS", RETURNS); ("END", END); ("DECLARE", DECLARE); 
    ("REFERENCES", REFERENCES); ("IN", IN); ("LANGUAGE", LANGUAGE);
    ("LOOP", LOOP); ("NULL", NULL); ("IF", IF); ("EXISTS", EXISTS);
    ("TABLE", TABLE); ("PRIMARY", PRIMARY); ("KEY", KEY); ("INDEX", INDEX);
    ("UNIQUE", UNIQUE); ("DEFAULT", DEFAULT); ("JOIN", JOIN); ("INNER", INNER);
    ("OUTER", OUTER); ("RIGHT", RIGHT); ("FULL", FULL); ("ON", ON);
    ("UPDATE", UPDATE); ("SET", SET); ("DELETE", DELETE); ("ALTER", ALTER);
    ("DROP", DROP); ("TRUNCATE", TRUNCATE); ("ORDER", ORDER); ("BY", BY);
    ("GROUP", GROUP); ("HAVING", HAVING); ("LIMIT", LIMIT); ("OFFSET", OFFSET);
    ("DISTINCT", DISTINCT); ("ALL", ALL); ("BETWEEN", BETWEEN); ("LIKE", LIKE);
    ("ILIKE", ILIKE); ("IS", IS);
  ]

let lookup_keyword s =
  try Hashtbl.find keyword_table (String.uppercase_ascii s)
  with Not_found -> ID s
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let int = '-'? digit+
let float = '-'? digit+ frac? exp? | '-'? digit* frac exp?

let idchar = ['a'-'z' 'A'-'Z' '_' '$']
let id = idchar (idchar | digit | '.' | '#')*

(* Simplified keyword patterns - we'll handle case insensitivity in code *)
let keyword = ['a'-'z' 'A'-'Z']+
let end_loop = "END" white+ "LOOP" | "end" white+ "loop" | "End" white+ "Loop"
let func_delim = ['$'] id* ['$']
let array_lit = ['['] [^ ']']* [']']
let colons = "::"
let parameter = '$' digit+
let json_op = "->" | "->>" | "#>" | "#>>"
let text_search_op = "@@" | "@@@"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | "/*"     { read_multiline_comment (Buffer.create 1024) lexbuf }
  | "--"     { read_comment (Buffer.create 1024) lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '\''      { read_sstring (Buffer.create 256) lexbuf }
  | '"'      { read_quoted_id_or_string (Buffer.create 256) lexbuf }
  | text_search_op { TEXT_SEARCH_OP (Lexing.lexeme lexbuf) }
  | json_op { JSON_OP (Lexing.lexeme lexbuf) }
  | parameter { PARAMETER (Lexing.lexeme lexbuf) }
  | func_delim     { FUNC_DELIM }
  | ">=" { GTE }
  | "<=" { LTE }
  | "<>" { NEQ }
  | "!=" { NEQ }
  | '>' { GT }
  | '<' { LT }
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
  | '*' { STAR }
  | ":=" { ASSIGN }
  | "=" { EQ }
  | end_loop { END_LOOP }
  | array_lit { ARRAY(Lexing.lexeme lexbuf) }
  | keyword { lookup_keyword (Lexing.lexeme lexbuf) }
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
  | '\'' '\'' { Buffer.add_char buf '\''; read_sstring buf lexbuf }
  | "\\'"     { Buffer.add_char buf '\''; read_sstring buf lexbuf }
  | [^ '\'']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_sstring buf lexbuf
    }
  | '\''       { SSTRING (Buffer.contents buf) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_quoted_id_or_string buf =
  parse
  | '\\' '/'  { Buffer.add_char buf '/'; read_quoted_id_or_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_quoted_id_or_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_quoted_id_or_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_quoted_id_or_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_quoted_id_or_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_quoted_id_or_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_quoted_id_or_string buf lexbuf }
  | '"' '"'   { Buffer.add_char buf '"'; read_quoted_id_or_string buf lexbuf }
  | "\\\""    { Buffer.add_char buf '"'; read_quoted_id_or_string buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_quoted_id_or_string buf lexbuf
    }
  | '"'       { 
      let content = Buffer.contents buf in
      (* Check if it looks like a string literal (contains spaces, special chars, etc.) *)
      if String.contains content ' ' || String.contains content ':' || String.contains content '.'
      then DSTRING content
      else QUOTED_ID content
    }
  | _ { raise (SyntaxError ("Illegal quoted character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Quoted identifier/string is not terminated")) }

and read_dstring buf =
  parse
  | '\\' '/'  { Buffer.add_char buf '/'; read_dstring buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_dstring buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_dstring buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_dstring buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_dstring buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_dstring buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_dstring buf lexbuf }
  | '"' '"'   { Buffer.add_char buf '"'; read_dstring buf lexbuf }
  | "\\\""    { Buffer.add_char buf '"'; read_dstring buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_dstring buf lexbuf
    }
  | '"'       { DSTRING (Buffer.contents buf) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_multiline_comment buf =
  parse
  | "*/"     { COMMENT (Buffer.contents buf) }
  | _
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_multiline_comment buf lexbuf
    }
  | eof { raise (SyntaxError ("Multiline comment is not terminated")) }

and read_comment buf =
  parse
  | newline     { INLINE_COMMENT (Buffer.contents buf) }
  | _
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_comment buf lexbuf
    }
  | eof { INLINE_COMMENT (Buffer.contents buf) }
