type expr =
  | Identifier of string
  | Literal of string

type stmt =
  | Select of expr list * string * string option

let fmt_opt = function
  | Some x -> Printf.sprintf "%s" x
  | None -> "None"

let show_expr = function
  | Identifier id -> id
  | Literal lit -> lit

let show_stmt = function
  | Select (fields, table, alias) ->
    let fields_str = fields |> List.map show_expr |> String.concat @@ Printf.sprintf "\n  , " in
    match alias with
      | Some alias -> Printf.sprintf "\nSELECT\n  %s \nFROM %s AS %s" fields_str table alias
      | None -> Printf.sprintf "\nSELECT\n  %s \nFROM %s" fields_str table
