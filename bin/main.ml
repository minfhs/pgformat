open Pgformat

let () =
  print_endline @@ Pgformatter.format_file "./fixture/select_simple.sql";
  print_endline @@ Pgformatter.format_file "./fixture/select.sql";
  print_endline @@ Pgformatter.format_file "./fixture/create.sql";
  print_endline @@ Pgformatter.format_file "./fixture/complex.sql"
