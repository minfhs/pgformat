open Core
open Ast

(*
   open Lexing
let _print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;*)

let parse_with_error lexbuf =
  try Some (Lexer.read lexbuf) with
  | SyntaxError msg ->
    Printf.eprintf "%s" msg;
    None
;;

let format_token indent_level indent_stack values_mode token before after =
  let indent rel =
    let ind = !indent_level in
    let ind = max 0 (ind + rel) in
    String.make (4 * ind) ' '
  in
  let nl () = printf "\n" in
  let indent_push () = Stack.push !indent_stack !indent_level in
  let indent_pop () =
    let ind = Option.value ~default:0 (Stack.pop !indent_stack) in
    indent_level := ind
  in
  let indent_inc () = indent_level := !indent_level + 1 in
  let indent_dec () = indent_level := !indent_level - 1 in
  let prt_ind_tok_spc ?(ind = 0) tok =
    printf "%s%s " (indent ind) (string_of_token tok)
  in
  let prt_ind_tok ?(ind = 0) tok = printf "%s%s" (indent ind) (string_of_token tok) in
  let prt_tok_spc tok = printf "%s " (string_of_token tok) in
  let prt_nltok tok = printf "\n%s" (string_of_token tok) in
  let prt_tok tok = printf "%s" (string_of_token tok) in
  let prt_ind () = printf "%s" (indent 0) in
  match before, token, after with
  | _, COMMENT c, _ -> printf "/*%s*/\n" c
  | _, INLINE_COMMENT _, _ -> prt_nltok token
  | _, VALUES, _ ->
    values_mode := true;
    indent_inc ();
    prt_tok_spc token
  | Some AS, FUNC_DELIM, _ -> prt_nltok token
  | _, SSTRING _, _ | _, DSTRING _, _ | _, COLONS, _ -> prt_tok token
  | _, EQ, _
  | _, INTO, _
  | _, ASSIGN, _
  | _, LANGUAGE, _
  | _, INT _, _
  | _, FLOAT _, _
  | _, FUNC_DELIM, _ -> prt_tok_spc token
  | _, AS, _ -> prt_tok_spc token
  | _, ARRAY a, _ -> printf "%s" a
  | _, SELECT, Some LEFT_PAREN -> prt_tok_spc token
  | _, LEFT_PAREN, Some RIGHT_PAREN -> prt_tok token
  | Some LEFT_PAREN, RIGHT_PAREN, _ -> prt_tok_spc token
  | _, ID id, next ->
    let space = if Poly.(next = Some SEMICOLON) || !values_mode then "" else " " in
    printf "%s%s" id space
  | None, CREATE, _ -> prt_tok_spc token
  | None, INSERT, _ | Some (COMMENT _), INSERT, _ -> prt_ind_tok_spc token
  | Some _, INSERT, _ | Some _, CREATE, _ ->
    nl ();
    prt_ind_tok_spc token
  | _, SEMICOLON, Some END_LOOP ->
    prt_tok token;
    nl ()
  | _, SEMICOLON, _ ->
    if !values_mode
    then (
      indent_dec ();
      values_mode := false);
    prt_tok token;
    nl ();
    prt_ind ()
  | Some RIGHT_PAREN, COMMA, Some LEFT_PAREN ->
    nl ();
    prt_ind_tok_spc token
  | _, AND, _ | _, LEFT, _ | _, RETURNS, _ | _, COMMA, _ ->
    if not !values_mode
    then (
      nl ();
      prt_ind_tok_spc token)
    else prt_tok_spc token
  | Some LEFT_PAREN, SELECT, _ ->
    prt_tok_spc token;
    nl ();
    indent_inc ();
    prt_ind ()
  | _, SELECT, _ ->
    prt_ind_tok_spc token;
    nl ();
    indent_inc ();
    prt_ind ()
  | _, LOOP, _ ->
    nl ();
    indent_pop ();
    prt_ind_tok_spc token;
    nl ();
    indent_inc ();
    prt_ind ()
  | _, DECLARE, _ ->
    nl ();
    prt_ind_tok_spc token;
    indent_inc ();
    nl ();
    prt_ind ()
  | _, IN, _ ->
    prt_tok_spc token;
    indent_push ();
    indent_inc ();
    nl ()
  | _, LEFT_PAREN, Some SELECT ->
    prt_tok_spc token;
    indent_push ();
    indent_inc ();
    nl ();
    prt_ind ()
  | _, LEFT_PAREN, _ ->
    prt_tok token;
    if not !values_mode
    then (
      indent_push ();
      indent_inc ();
      nl ();
      prt_ind ())
  | _, RIGHT_PAREN, next ->
    if not !values_mode
    then (
      nl ();
      indent_pop ();
      prt_ind_tok token)
    else prt_tok token;
    if Poly.(next = Some SEMICOLON) then () else printf "%s" " "
  | _, END_LOOP, _ ->
    indent_pop ();
    prt_ind_tok ~ind:1 token
  | _, BEGIN, _ ->
    indent_dec ();
    nl ();
    prt_ind_tok_spc token;
    nl ();
    indent_inc ();
    prt_ind ()
  | _, FROM, _ ->
    indent_dec ();
    nl ();
    prt_ind_tok_spc token
  | _, WHERE, _ ->
    nl ();
    prt_ind_tok_spc token;
    nl ();
    indent_inc ();
    prt_ind ()
  | _, _, _ -> ()
;;

let take2 = function
  | a :: b :: _ -> [ a; b ]
  | lst -> lst
;;

let rec parse indent stack values_mode lexbuf tokens =
  let nxt_tok = parse_with_error lexbuf in
  (match tokens with
   | cur_tok :: prv_tok :: _ ->
     format_token indent stack values_mode cur_tok (Some prv_tok) nxt_tok
   | cur_tok :: _ -> format_token indent stack values_mode cur_tok None nxt_tok
   | _ -> ( (*keep parsing*) ));
  match nxt_tok with
  | Some EOF -> (*purge parser*) format_token indent stack values_mode EOF None None
  | Some tok -> parse indent stack values_mode lexbuf (take2 (tok :: tokens))
  | None -> ()
;;

let format lexbuf =
  let stack = ref (Stack.create ()) in
  let indent = ref 0 in
  let values_mode = ref false in
  let _ = Stack.push !stack !indent in
  parse indent stack values_mode lexbuf []
;;

let format_stdio () =
  let inx = In_channel.stdin in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "STDIN" };
  let _ = format lexbuf in
  In_channel.close inx
;;

let format_file filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let _ = format lexbuf in
  In_channel.close inx
;;
