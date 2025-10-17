open Core

(* Output interface for formatting *)
module type Output = sig
  val print_string : string -> unit
  val print_newline : unit -> unit
end

module PrintOutput : Output = struct
  let print_string s = printf "%s" s
  let print_newline () = printf "\n"
end

(* String buffer output for testing *)
module BufferOutput (B : sig val buf : Buffer.t end) : Output = struct
  let print_string s = Buffer.add_string B.buf s
  let print_newline () = Buffer.add_char B.buf '\n'
end
