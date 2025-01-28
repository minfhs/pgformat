{
  open Parser
}

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '.']* 
let idas = id (' ' 'a' 's' ' ') id

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* Skip whitespace *)
  | "SELECT" { SELECT }
  | "FROM" { FROM }
  | "AS" { AS }
  | id as id { ID id }
  | idas as id { ID id }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | _ { raise (Failure ("Unknown token: " ^ Lexing.lexeme lexbuf)) }
