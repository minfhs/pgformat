open Pgcore.Ast

(* Alias modules for convenience *)
module Output = Pgcore.Output
module Config = Pgcore.Config

(* Comment formatting functionality *)
module CommentFormatter (O : Output.Output) (Config : sig val config : Config.format_config end) = struct
  module PrintHelpers = Pgcore.Print_helpers.PrintHelpers (O) (Config)
  open PrintHelpers

  let format_comment _state comment next_token =
    match next_token with
    | Some SELECT | Some INSERT -> print_string ("/*" ^ comment ^ "*/"); print_newline ()
    | _ -> print_string ("/*" ^ comment ^ "*/")

  let format_inline_comment _state _comment =
    print_token _comment;
    print_newline ()
end
