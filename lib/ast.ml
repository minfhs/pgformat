exception SyntaxError of string

type t =
  | ID of string
  | QUOTED_ID of string
  | INT of int
  | FLOAT of float
  | SSTRING of string
  | DSTRING of string
  | PARAMETER of string
  | JSON_OP of string
  | TEXT_SEARCH_OP of string
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
  | GTE
  | LTE
  | NEQ
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
  | OR
  | NOT
  | FROM
  | INTO
  | VALUES
  | REFERENCES
  | WHERE
  | CREATE
  | INSERT
  | LEFT
  | RIGHT
  | INNER
  | OUTER
  | FULL
  | JOIN
  | ON
  | IF
  | EXISTS
  | TABLE
  | PRIMARY
  | KEY
  | INDEX
  | UNIQUE
  | DEFAULT
  | UPDATE
  | SET
  | DELETE
  | ALTER
  | DROP
  | TRUNCATE
  | ORDER
  | BY
  | GROUP
  | HAVING
  | LIMIT
  | OFFSET
  | DISTINCT
  | ALL
  | BETWEEN
  | LIKE
  | ILIKE
  | IS
  | FUNC_DELIM
  | ASSIGN
  | EQ
  | PLUS
  | MINUS
  | STAR
  | RETURNS
  | END
  | DECLARE
  | LANGUAGE
  | TODO

let string_of_token = function
  | ID x -> x
  | QUOTED_ID x -> Printf.sprintf {|"%s"|} x
  | INT x -> string_of_int x
  | FLOAT x -> string_of_float x
  | SSTRING x -> Printf.sprintf {|'%s'|} x
  | DSTRING x -> Printf.sprintf {|"%s"|} x
  | PARAMETER x -> x
  | JSON_OP x -> x
  | TEXT_SEARCH_OP x -> x
  | INLINE_COMMENT x -> Printf.sprintf {|-- %s|} (String.trim x)
  | SELECT -> "SELECT"
  | AS -> "AS"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | BEGIN -> "BEGIN"
  | IN -> "IN"
  | FROM -> "FROM"
  | VALUES -> "VALUES"
  | INTO -> "INTO"
  | WHERE -> "WHERE"
  | INSERT -> "INSERT"
  | CREATE -> "CREATE"
  | LEFT -> "LEFT"
  | RIGHT -> "RIGHT"
  | INNER -> "INNER"
  | OUTER -> "OUTER"
  | FULL -> "FULL"
  | JOIN -> "JOIN"
  | ON -> "ON"
  | IF -> "IF"
  | EXISTS -> "EXISTS"
  | TABLE -> "TABLE"
  | PRIMARY -> "PRIMARY"
  | KEY -> "KEY"
  | INDEX -> "INDEX"
  | UNIQUE -> "UNIQUE"
  | DEFAULT -> "DEFAULT"
  | UPDATE -> "UPDATE"
  | SET -> "SET"
  | DELETE -> "DELETE"
  | ALTER -> "ALTER"
  | DROP -> "DROP"
  | TRUNCATE -> "TRUNCATE"
  | ORDER -> "ORDER"
  | BY -> "BY"
  | GROUP -> "GROUP"
  | HAVING -> "HAVING"
  | LIMIT -> "LIMIT"
  | OFFSET -> "OFFSET"
  | DISTINCT -> "DISTINCT"
  | ALL -> "ALL"
  | BETWEEN -> "BETWEEN"
  | LIKE -> "LIKE"
  | ILIKE -> "ILIKE"
  | IS -> "IS"
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
  | STAR -> "*"
  | LT -> "<"
  | GT -> ">"
  | GTE -> ">="
  | LTE -> "<="
  | NEQ -> "<>"
  | NULL -> "NULL"
  | ARRAY a -> a
  | LANGUAGE -> "LANGUAGE"
  | EOF -> "\n"
  | _ -> failwith "NO"
;;
