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

let format_string sql =
  let buffer = Buffer.create 2048 in  (* Create a buffer *)
  let formatter = Format.formatter_of_buffer buffer in
  let printf format = Format.fprintf formatter format in
  let sql = Str.global_replace (Str.regexp ",") " , " sql in
  let sql = Str.global_replace (Str.regexp "(") " ( " sql in
  let sql = Str.global_replace (Str.regexp ")") " ) " sql in
  let sql = Str.global_replace (Str.regexp "\\[") " [ " sql in
  let sql = Str.global_replace (Str.regexp "]") " ] " sql in
  let sql = Str.global_replace (Str.regexp ";") " ; " sql in
  let sql = Str.global_replace (Str.regexp "[\t]+") " " sql in
  let sql = Str.global_replace (Str.regexp "\n") " " sql in
  let sql = Str.global_replace (Str.regexp "[ ]+") " " sql in
  (* Special cases *)
  let sql = Str.global_replace (Str.regexp "END LOOP") " __ENDLOOP " sql in
  let sql = Str.global_replace (Str.regexp "( )") " __EMPTY_PAREN " sql in
  let parts = Str.split (Str.regexp " ") sql in
  let indent = ref 0 in
  let raw_mode = ref false in
  List.iteri parts ~f:(fun i word ->
    let ind () =
      if !indent < 0 then indent := 0;
      String.make (!indent*4) ' ' in
    if !raw_mode then
      let () = if String.equal "]" word then raw_mode := false in
      match word with
      | "__EMPTY_PAREN" -> printf "()"
      | "," -> printf ", "
      | "+" -> printf " + "
      | _ -> printf "%s" word
    else match String.uppercase (String.strip word) with
      | "SELECT" -> 
        printf "\n%sSELECT\n" (ind());
        indent := !indent + 1;
        printf "%s" (ind())
      | "WHERE" -> 
        printf "\n%sWHERE\n    %s" (ind()) (ind());
        indent := !indent + 1;
      | "CREATE" -> printf "\n%sCREATE " (ind())
      | "RETURNS" -> printf "\n%sRETURNS " (ind())
      | "LEFT" -> printf "\n%sLEFT " (ind())
      | "BEGIN" -> printf "\nBEGIN\n%s" (ind()); indent := 1 
      | "END" -> indent := !indent - 1; printf "\n%sEND" (ind())
      | "IN" ->
        printf "IN\n%s" (ind());
        indent := !indent + 1;
        printf "%s" (ind())
      | "FROM" ->
        indent := !indent - 1;
        printf "\n%sFROM " (ind());
      | "LOOP" ->
        indent := !indent - 1;
        printf "\n%sLOOP\n" (ind());
        indent := !indent + 1;
        printf "%s" (ind())
      | "DECLARE" ->
        printf "\n%sDECLARE\n" (ind());
        indent := !indent + 1;
        printf "%s" (ind())
      | "VALUES" -> printf " VALUES "
      | "AS" -> printf " AS "
      | "AND" ->
        printf "\n%sAND " (ind());
      | "__EMPTY_PAREN" -> printf "()"
      | "__ENDLOOP" ->
        indent := !indent - 1;
        printf "\n%sEND LOOP" (ind());
      | "[" -> raw_mode := true; printf "["
      | ";" -> printf ";\n%s" (ind ())
      | "," -> printf "\n%s, " (ind ())
      | "(" -> indent := !indent + 1; printf "(\n%s" (ind ())
      | ")" -> 
        indent := !indent - 1;
        if (is_nth_equal parts (i + 1) "AND") then 
          indent := !indent - 1;
        printf "\n%s)" (ind ())
      | _ ->  printf "%s " word
  ); 

  Format.pp_print_flush formatter ();
  let sql = Buffer.contents buffer in
  let sql = Str.global_replace (Str.regexp " \n") "\n" sql in
  let sql = Str.global_replace (Str.regexp "[ ]+;") ";" sql in
  let sql = Str.global_replace (Str.regexp "[ ]+AS") " AS" sql in
  let sql = Str.global_replace (Str.regexp "^[ \t]*$") "" sql in
  let sql = Str.global_replace (Str.regexp "\n+") "\n" sql in
  sql
  

let format_file file =
  let sql = In_channel.read_all @@ sprintf file in
  format_string sql
