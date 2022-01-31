open Parserlib.Types
open Parserlib.Errors
open Parserlib.Combinators

open Alcotest

let parser_result_stringify ppf = function
    | Ok (c,rest) -> Fmt.pf ppf "%c,%s" c rest
    | Error (_,rest) -> Fmt.pf ppf "%s" rest

let parser_result_ttb = testable parser_result_stringify (=)

let parser_expect_equal expected actual () = 
  (check parser_result_ttb) "Expected values to be equal" expected actual

let () =
  run "Parserlib Tests"
    [
      ("get_char tests",
      [
        test_case "test_single_char" `Quick (parser_expect_equal (ok 'a' "b") (get_char "ab"));
        test_case "test_no_char" `Quick (parser_expect_equal (error (EndOfInputError) "") (get_char ""));
        test_case "test_only_one_char" `Quick ((parser_expect_equal (ok 'a' "") (get_char "a"))) 
      ])
    ]
