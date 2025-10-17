open Core

(* Custom output module that captures to a buffer *)
let make_buffer_output buffer = 
  let module BufferOutput = struct
    let print_string s = Buffer.add_string buffer s
    let print_newline () = Buffer.add_char buffer '\n'
  end in
  (module BufferOutput : Pgformat.Output.Output)

(* Helper function to format with custom config and capture output *)
let format_with_custom_config config sql =
  let buffer = Buffer.create 256 in
  let output_module = make_buffer_output buffer in
  
  let builder = Pgformat.Builder.FormatterBuilder.create ()
    |> Pgformat.Builder.FormatterBuilder.with_indent_size config.Pgformat.Config.indent_size
    |> Pgformat.Builder.FormatterBuilder.with_max_line_length config.Pgformat.Config.max_line_length
    |> Pgformat.Builder.FormatterBuilder.with_output output_module in
  
  let temp_file, fd = Core_unix.mkstemp "temp.XXXXXX" in
  Core_unix.close fd;
  
  Exn.protect
    ~f:(fun () ->
      (* Write SQL to temp file *)
      Out_channel.with_file temp_file ~f:(fun oc ->
        Out_channel.output_string oc sql);
      
      (* Format using custom builder *)
      In_channel.with_file temp_file ~f:(fun ic ->
        let lexbuf = Lexing.from_channel ic in
        Pgformat.Builder.FormatterBuilder.format_with_config builder lexbuf);
      
      Buffer.contents buffer)
    ~finally:(fun () ->
      try Core_unix.unlink temp_file with _ -> ())

let%expect_test "custom_formatter_with_2_space_indent" =
  let sql = "SELECT a,b FROM table_name;" in
  
  (* Create a custom config with 2-space indent *)
  let config = { Pgformat.Config.default_config with indent_size = 2 } in
  
  let output = format_with_custom_config config sql in
  print_string output;
  [%expect
    {|
    SELECT
      a
      , b
    FROM table_name;
    |}]

let%expect_test "custom_formatter_with_8_space_indent" =
  let sql = "SELECT a,b FROM table_name;" in
  
  (* Create a custom config with 8-space indent *)
  let config = { Pgformat.Config.default_config with indent_size = 8 } in
  
  let output = format_with_custom_config config sql in
  print_string output;
  [%expect
    {|
    SELECT
            a
            , b
    FROM table_name;
    |}]

(* Simple test that just verifies the builder doesn't crash *)
let%test "builder_basic_functionality" =
  let sql = "SELECT 1;" in
  
  let builder = Pgformat.Builder.FormatterBuilder.create ()
    |> Pgformat.Builder.FormatterBuilder.with_indent_size 2
    |> Pgformat.Builder.FormatterBuilder.with_max_line_length 80 in
  
  let temp_file, fd = Core_unix.mkstemp "temp.XXXXXX" in
  Core_unix.close fd;
  
  Exn.protect
    ~f:(fun () ->
      Out_channel.with_file temp_file ~f:(fun oc ->
        Out_channel.output_string oc sql);
      
      In_channel.with_file temp_file ~f:(fun ic ->
        let lexbuf = Lexing.from_channel ic in
        Pgformat.Builder.FormatterBuilder.format_with_config builder lexbuf);
      
      true)
    ~finally:(fun () ->
      try Core_unix.unlink temp_file with _ -> ())
