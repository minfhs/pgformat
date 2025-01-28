open Pgformat
open Alcotest

let parse_sql input =
  let lexbuf = Lexing.from_string input in
  try
    Ok (Parser.stmt Lexer.token lexbuf)
  with
  | Failure msg -> Error msg
  | Parser.Error -> Error "Syntax error"

(* Pretty-printer for Ast.expr *)
let _pp_expr fmt = function
  | Ast.Identifier id -> Format.fprintf fmt "Identifier(%s)" id
  | _ -> Format.fprintf fmt ""

(* Pretty-printer for Ast.stmt *)
let pp_stmt fmt = function
  | Ast.Select (fields, table) ->
    let fields_str = fields |> List.map Ast.show_expr |> String.concat ", " in
    Format.fprintf fmt "SELECT %s FROM %s" fields_str table

(* Testable values for Alcotest *)
let _expression = testable _pp_expr (=)
let statement = testable pp_stmt (=)

let test_select_stmt () =
  let sql = "SELECT id, name FROM users;" in
  match parse_sql sql with
  | Ok (Some stmt) ->
    let expected_stmt = Ast.Select ([Ast.Identifier "id"; Ast.Identifier "name"], "users") in
    check statement "parsed statement" expected_stmt stmt 
  | Ok None -> failwith "Parsing failed"
  | Error msg -> failwith msg

let () =
  run "SQL Parser" [
    ("SELECT statement", [
      test_case "Parses SELECT statement" `Quick test_select_stmt;
    ]);
  ]
