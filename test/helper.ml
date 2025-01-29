open Pgformat
let format_script sql = print_endline (Pgformatter.format_string sql)
