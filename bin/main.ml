open Pgformat

let parse_sql input =
  let lexbuf = Lexing.from_string input in
  try
    Ok (Parser.stmt Lexer.token lexbuf)
  with
  | Failure msg -> Error msg
  | Parser.Error -> Error "Syntax error"

let () =
  let sql = "SELECT users.id as id, name FROM users AS usrs;" in
  match parse_sql sql with
  | Ok (Some stmt) -> Printf.printf "Parsed SQL: %s\n" (Ast.show_stmt stmt)
  | Ok None -> Printf.eprintf "Error: Failed to parse SQL\n"
  | Error msg -> Printf.eprintf "Error: %s\n" msg
