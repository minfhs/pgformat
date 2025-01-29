open! Core
open Helper

let%expect_test "Simple SELECT" =
  format_script "SELECT a,b FROM x as y;";
  [%expect {|
    SELECT
        a
        , b
    FROM x  AS y;
    |}]
;;
