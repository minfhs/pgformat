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
  | CREATE -> "CREATE"
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
  | EOF -> "\n"
  | _ -> failwith "NO"
;;

let parse_with_error lexbuf =
  try Some (Lexer.read lexbuf) with
  | SyntaxError msg ->
    Printf.eprintf "%s" msg;
    None
;;

let format_token indent_stack token before after =
  let indent rel =
    let ind = Stack.top !indent_stack in
    String.make (4 * (Option.value ind ~default:0 + rel)) ' '
  in
  let tabs () = indent 0 in
  let prt token = printf "%s%s " (tabs ()) (string_of_token token) in
  let nl () = printf "\n" in
  let increase_indent_push indent_stack =
    let ind = Stack.top !indent_stack |> Option.value ~default:0 in
    Stack.push !indent_stack (ind + 1)
  in
  let decrease_indent_pop indent_stack =
    let _ = Stack.pop !indent_stack in
    ()
  in
  match before, token, after with
  | _, SELECT, Some LEFT_PAREN -> prt token
  | _, SELECT, _ ->
    increase_indent_push indent_stack;
    prt token;
    nl ();
    printf "%s" (indent 0) (* prepare next token indented*)
  | _, COMMA, _ ->
    nl ();
    prt token
  | _, LEFT_PAREN, _ ->
    prt token;
    increase_indent_push indent_stack;
    nl ();
    printf "%s" (indent 0)
  | _, RIGHT_PAREN, _ ->
    decrease_indent_pop indent_stack;
    nl ();
    printf "%s%s" (tabs ()) (string_of_token token);
    printf "%s" (indent 0)
  | _, FROM, _ ->
    nl ();
    printf "%s%s " (indent (-1)) (string_of_token token)
  | _, WHERE, _ | _, INSERT, _ | Some _, CREATE, _ ->
    printf "\n%s%s " (tabs ()) (string_of_token token)
  | _, _, _ -> printf "%s " (string_of_token token)
;;

let rec parse stack lexbuf tokens =
  let tok = parse_with_error lexbuf in
  (match tokens with
   | cur :: prev :: _ -> format_token stack cur (Some prev) tok
   | cur :: _ -> format_token stack cur None tok
   | _ -> ());
  match tok with
  | Some EOF ->
    format_token stack EOF None None;
    EOF :: tokens
  | Some tok -> parse stack lexbuf (tok :: tokens)
  | None -> tokens
;;

let loop filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let stack = ref (Stack.create ()) in
  let _ = Stack.push !stack 0 in
  let _tokens = parse stack lexbuf [] in
  (* let tokens = List.rev tokens in *)
  (* List.iteri tokens ~f:(fun i t -> *)
  (* format_token stack t (List.nth tokens (i - 1)) (List.nth tokens (i + 1))); *)
  In_channel.close inx
;;

let () = loop "./fixture/create.sql"

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
