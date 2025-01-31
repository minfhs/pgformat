open Pgformat
open Core
open Lexer
open Lexing

let _print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let string_of_token = function
  | ID x -> x
  | INT x -> string_of_int x
  | STRING x -> x
  | SELECT -> "SELECT"
  | AS -> "AS"
  | FROM -> "FROM"
  | VALUES -> "VALUES"
  | INTO -> "INTO"
  | WHERE -> "WHERE"
  | INSERT -> "INSERT"
  | COMMA -> ","
  | SEMICOLON -> ";"
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | LEFT_BRACK -> "["
  | RIGHT_BRACK -> "]"
  | COLON -> ":"
  | FUNC_DELIM -> "$$"
  | ASSIGN -> ":="
  | EQ -> "="
  | PLUS -> "+"
  | MINUS -> "-"
  | _ -> failwith "NO"
;;

let parse_with_error lexbuf =
  try Some (Lexer.read lexbuf) with
  | SyntaxError msg ->
    Printf.eprintf "%s" msg;
    None
;;

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some token ->
    (match token with
     | EOF -> ()
     | SELECT | FROM | WHERE | INSERT ->
       printf "\n%s\n" (string_of_token token);
       parse_and_print lexbuf
     | _ ->
       printf "%s " (string_of_token token);
       parse_and_print lexbuf)
  | None -> ()
;;

let loop filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx
;;

let () = loop "./fixture/complex.sql"

(* let format_content = function *)
(* | None | Some "-" -> In_channel.input_all In_channel.stdin |> Pgformatter.format_string *)
(* | Some file -> Pgformatter.format_file file *)
(* ;; *)

(* let command = *)
(* Command.basic *)
(* ~summary:"File to format" *)
(* ~readme:(fun () -> "Path to .sql file that you want to format with pgformat") *)
(* (let%map_open.Command write = flag "-w" no_arg ~doc:" write to file" *)
(* and filedesc = anon (maybe ("filename" %: string)) in *)
(* fun () -> *)
(* let data = format_content filedesc in *)
(* if write then match filedesc with *)
(* | Some file -> Out_channel.write_all file ~data *)
(* | None -> Printf.eprintf "You can only write to file when a file is passed" *)
(* else Printf.printf "%s" data) *)
(* ;; *)

(* let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command *)
