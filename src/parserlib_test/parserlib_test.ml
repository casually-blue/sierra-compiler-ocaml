open Parserlib.Types
open Parserlib.Errors
open Parserlib.Combinators

open Alcotest

let parser_result_ttb = testable 
  (fun pftr -> Format.fprintf pftr "%a" (pp_parser_result Format.pp_print_char))
  (=)

let expect_equal ty expected actual () = 
  (check ty) "Expected values to be equal" expected actual

let parser_expect_equal = expect_equal parser_result_ttb

let () =
  run "Parserlib Tests"
    [
      ("get_char tests",
      [
        test_case "test_single_char" `Quick (parser_expect_equal (ok 'a' "b") (get_char "ab"));
        test_case "test_no_char" `Quick (parser_expect_equal (error (EndOfInputError) "") (get_char ""));
        test_case "test_only_one_char" `Quick ((parser_expect_equal (ok 'a' "") (get_char "a"))) 
      ]);
      ("match_char tests",
      [
        test_case "test_match_a" `Quick 
          (parser_expect_equal 
            (ok 'a' "b") 
            ((match_char (fun c -> c == 'a') (ExpectationError "a")) "ab")
          );
        test_case "test_error_a" `Quick
          (parser_expect_equal
            (error (ExpectationError "a") "bb")
            ((match_char (fun c -> c == 'a') (ExpectationError "a")) "bb"))
      ])
    ]
