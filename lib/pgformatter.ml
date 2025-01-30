open Core

let is_nth_equal lst n target =
  (* Helper function to safely get the nth element *)
  let rec get_nth lst n =
    match lst with
    | [] -> None
    | hd :: tl -> if n = 0 then Some hd else get_nth tl (n - 1)
  in
  match get_nth lst n with
  | Some item -> String.equal item target
  | None -> false
;;

let values_mode_parse
      prev
      next
      word
      values_mode
      (printf : ('a, Format.formatter, unit) format -> 'a)
      tabs
  =
  if String.(next = ";") then values_mode := false;
  match word with
  | "," ->
    if String.(prev = ")") && String.(next = "(")
    then printf "\n%s    %s " tabs word
    else printf "%s%s" word " "
  | _ -> printf "%s%s" word ""
;;

let raw_mode_parse word raw_mode (printf : ('a, Format.formatter, unit) format -> 'a) =
  let () = if String.equal "]" word then raw_mode := false in
  match word with
  | "__EMPTY_PAREN" -> printf "%s" "()"
  | "," -> printf "%s" ", "
  | "+" -> printf "%s" " + "
  | _ -> printf "%s" word
;;

let format_string sql =
  let buffer = Buffer.create 2048 in
  (* Create a buffer *)
  let formatter = Format.formatter_of_buffer buffer in
  let printf format = Format.fprintf formatter format in
  let tokenize regmatch replacer inp =
    Str.global_replace (Str.regexp regmatch) replacer inp
  in
  let sql =
    sql
    |> tokenize "]" " ] "
    |> tokenize {|\([;,()\[]\)|} {| \0 |}
    |> tokenize "[\t\n ]+" " "
    |> tokenize "END LOOP" " __ENDLOOP "
    |> tokenize "( )" " __EMPTY_PAREN "
  in
  let parts = Str.split (Str.regexp " ") sql in
  let indent = ref 0 in
  let parenlevel = ref (Stack.create ()) in
  let where_clause = ref false in
  let raw_mode = ref false in
  let values_mode = ref false in
  List.iteri parts ~f:(fun i word ->
    let ind () =
      if !indent < 0 then indent := 0;
      String.make (!indent * 4) ' '
    in
    let tabs = ind () in
    if !raw_mode
    then raw_mode_parse word raw_mode printf
    else if !values_mode
    then
      values_mode_parse
        (Option.value ~default:" " (List.nth parts (i - 1)))
        (Option.value ~default:" " (List.nth parts (i + 1)))
        word
        values_mode
        printf
        tabs
    else (
      match String.uppercase (String.strip word) with
      | "SELECT" ->
        printf "\n%sSELECT" tabs;
        if not (is_nth_equal parts (i + 1) "(")
        then (
          let () = indent := !indent + 1 in
          printf "\n%s" (ind ()))
        else printf " "
      | "WHERE" ->
        where_clause := true;
        printf "\n%sWHERE\n    %s" tabs tabs;
        indent := !indent + 1
      | "CREATE" -> printf "\n%sCREATE " tabs
      | "RETURNS" -> printf "\n%sRETURNS " tabs
      | "LEFT" -> printf "\n%sLEFT " tabs
      | "BEGIN" ->
        printf "\nBEGIN\n%s" tabs;
        indent := 1
      | "END" ->
        indent := !indent - 1;
        printf "\n%sEND" (ind ())
      | "IN" ->
        printf "IN\n%s" tabs;
        indent := !indent + 1;
        printf "%s" (ind ())
      | "FROM" ->
        indent := !indent - 1;
        printf "\n%sFROM " (ind ())
      | "LOOP" ->
        indent := !indent - if !where_clause then 2 else 1;
        where_clause := false;
        printf "\n%sLOOP\n" (ind ());
        indent := !indent + 1;
        printf "%s" (ind ())
      | "DECLARE" ->
        printf "\n%sDECLARE\n" tabs;
        indent := !indent + 1;
        printf "%s" (ind ())
      | "VALUES" ->
        values_mode := true;
        printf " VALUES "
      | "AS" -> printf " AS "
      | "AND" -> printf "\n%sAND " tabs
      | "__EMPTY_PAREN" -> printf "()"
      | "__ENDLOOP" ->
        indent := !indent - 1;
        printf "\n%sEND LOOP" (ind ())
      | "[" ->
        raw_mode := true;
        printf "["
      | ";" ->
        values_mode := false;
        printf ";\n%s" tabs
      | "," -> printf "\n%s, " tabs
      | "(" ->
        Stack.push !parenlevel !indent;
        indent := !indent + 1;
        printf "(\n%s" (ind ())
      | ")" ->
        indent := Option.value (Stack.pop !parenlevel) ~default:0;
        where_clause := false;
        printf "\n%s)" (ind ())
      | _ -> printf "%s " word));
  Format.pp_print_flush formatter ();
  let sql = Buffer.contents buffer in
  let sql = Str.global_replace (Str.regexp " \n") "\n" sql in
  let sql = Str.global_replace (Str.regexp "[ ]+;") ";" sql in
  let sql = Str.global_replace (Str.regexp "[ ]+AS") " AS" sql in
  let sql = Str.global_replace (Str.regexp "^[ \t]*$") "" sql in
  let sql = Str.global_replace (Str.regexp "\n+") "\n" sql in
  let sql = String.strip sql in
  sql
;;

let format_file (file : string) =
  let sql = In_channel.read_all file in
  format_string sql
;;
