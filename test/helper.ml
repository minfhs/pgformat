open Core
open Core_unix
open Pgformat

(* let format_script sql = print_endline (Pgformatter.format_string sql) *)

let format_script sql =
  let temp_file, fd = mkstemp "temp.XXXXXX" in
  close fd;
  Exn.protect
    ~f:(fun () ->
      Out_channel.with_file temp_file ~f:(fun oc -> Out_channel.fprintf oc sql);
      Pgformatter.format_file temp_file)
    ~finally:(fun () ->
      try unlink temp_file with
      | _ -> ())
;;
