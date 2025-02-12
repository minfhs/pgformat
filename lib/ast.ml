exception SyntaxError of string

type t =
  | ID of string
  | INT of int
  | FLOAT of float
  | SSTRING of string
  | DSTRING of string
  | ARRAY of string
  | COMMENT of string
  | INLINE_COMMENT of string
  | NULL
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACK
  | RIGHT_BRACK
  | LT
  | GT
  | COLON
  | COLONS
  | SEMICOLON
  | COMMA
  | EOF
  | BEGIN
  | SELECT
  | AS
  | IN
  | LOOP
  | END_LOOP
  | AND
  | FROM
  | INTO
  | VALUES
  | REFERENCES
  | WHERE
  | CREATE
  | INSERT
  | LEFT
  | FUNC_DELIM
  | ASSIGN
  | EQ
  | PLUS
  | MINUS
  | RETURNS
  | END
  | DECLARE
  | LANGUAGE
  | TODO

let string_of_token = function
  | ID x -> x
  | INT x -> string_of_int x
  | FLOAT x -> string_of_float x
  | SSTRING x -> Printf.sprintf {|'%s'|} x
  | DSTRING x -> Printf.sprintf {|"%s"|} x
  | INLINE_COMMENT x -> Printf.sprintf {|-- %s|} x
  | SELECT -> "SELECT"
  | AS -> "AS"
  | AND -> "AND"
  | BEGIN -> "BEGIN"
  | IN -> "IN"
  | FROM -> "FROM"
  | VALUES -> "VALUES"
  | INTO -> "INTO"
  | WHERE -> "WHERE"
  | INSERT -> "INSERT"
  | CREATE -> "CREATE"
  | LEFT -> "LEFT"
  | COMMA -> ","
  | COLONS -> "::"
  | SEMICOLON -> ";"
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | LEFT_BRACK -> "["
  | RIGHT_BRACK -> "]"
  | COLON -> ":"
  | RETURNS -> "RETURNS"
  | REFERENCES -> "REFERENCES"
  | END -> "END"
  | LOOP -> "LOOP"
  | END_LOOP -> "END LOOP"
  | FUNC_DELIM -> "$$"
  | DECLARE -> "DECLARE"
  | ASSIGN -> ":="
  | EQ -> "="
  | PLUS -> "+"
  | MINUS -> "-"
  | LT -> "<"
  | GT -> ">"
  | NULL -> "NULL"
  | ARRAY a -> a
  | LANGUAGE -> "LANGUAGE"
  | EOF -> "\n"
  | _ -> failwith "NO"
;;
