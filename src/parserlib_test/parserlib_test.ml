open Parserlib.Types
open Parserlib.Errors
open Parserlib.Combinators

open Alcotest

let parser_result_stringify ppf = function
    | Ok (c,rest) -> Fmt.pf ppf "%c,%s" c rest
    | Error (_,rest) -> Fmt.pf ppf "%s" rest

let parser_result_ttb = testable parser_result_stringify (=)

  let test_onechar () = 
    (check parser_result_ttb) "Same result" (ok 'a' "b") (get_char "ab")
  let test_nochar () = 
    (check parser_result_ttb) "Error no value" (error (EndOfInputError) "") (get_char "")

let () =
  run "Parserlib Tests"
    [
      ("get_char tests",
      [
        test_case "test_single_char" `Quick test_onechar;
        test_case "test_no_char" `Quick test_nochar
      ])
    ]
