open Ast
open Output
open Config
open Print_helpers

(* Comment formatting functionality *)
module CommentFormatter (O : Output) (Config : sig val config : format_config end) = struct
  module PrintHelpers = PrintHelpers (O) (Config)
  open PrintHelpers

  let format_comment _state comment next_token =
    match next_token with
    | Some SELECT | Some INSERT -> print_string ("/*" ^ comment ^ "*/"); print_newline ()
    | _ -> print_string ("/*" ^ comment ^ "*/")

  let format_inline_comment _state _comment =
    print_token _comment;
    print_newline ()
end
