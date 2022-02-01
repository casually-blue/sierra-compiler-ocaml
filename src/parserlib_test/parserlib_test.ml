open Parserlib.Types
open Parserlib.Errors
open Parserlib.Combinators

open Alcotest

let parser_result_ttb pretty_printer = testable 
  (fun pftr -> Format.fprintf pftr "%a" (pp_parser_result pretty_printer))
  (=)

let expect_equal ty expected actual () = 
  (check ty) "Expected values to be equal" expected actual

let char_parser_expect_equal = expect_equal (parser_result_ttb Format.pp_print_char)

let pp_print_unit = (fun fmtr () -> Format.fprintf fmtr "()")

let unit_parser_expect_equal = expect_equal (parser_result_ttb pp_print_unit)
let char_list_parser_expect_equal = expect_equal (parser_result_ttb (Format.pp_print_list Format.pp_print_char))

let quick_tcase ex_equal_fn name expected actual = 
  test_case ("test_" ^ name) `Quick (ex_equal_fn expected actual)

let () =
  run "Parserlib Tests"
    [

      ("get_char tests",
      let tcase = quick_tcase char_parser_expect_equal in [
        tcase "single_char"
          (ok 'a' "b")
          (get_char "ab");
        tcase "no_char"
          (error (EndOfInputError) "")
          (get_char "");
        tcase "only_one_char"
          (ok 'a' "") 
          (get_char "a")
      ]);

      ("match_char tests",
      let tcase = quick_tcase char_parser_expect_equal in 
      let match_a = (match_char ((=) 'a') (ExpectationError "a")) in [
        tcase "match_a" 
          (ok 'a' "b")
          (match_a "ab");
        tcase "error_a"
          (error (ExpectationError "a") "bb")
          (match_a "bb")
      ]);

      ("antimatch_char tests",
      let tcase = quick_tcase char_parser_expect_equal in
      let antimatch_b = (antimatch_char ((=) 'b') (ExpectationError "not b")) in [
        tcase "nomatch_a"
          (ok 'a' "b")
          (antimatch_b "ab");
        tcase "nomatch_a_error"
          (error (ExpectationError "not b") "bb")
          (antimatch_b "bb")
      ]);

      ("eof tests",
      let tcase = quick_tcase unit_parser_expect_equal in [
        tcase "eof"
          (ok () "")
          (eof "");
        tcase "eof_error"
          (error (ExpectationError "end of input") "a")
          (eof "a")
      ]);

      ("charp tests",
      let tcase = quick_tcase char_parser_expect_equal in
      let match_a = (charp 'a') in [ 
        tcase "just_a"
          (ok 'a' "")
          (match_a "a");
        tcase "a_with_rest"
          (ok 'a' "b")
          (match_a "ab")
      ]);

      ("basic many tests",
      let tcase = quick_tcase char_list_parser_expect_equal in
      let many_a = (many (charp 'a')) in [
        tcase "no_a"
          (ok [] "")
          (many_a "");
        tcase "one_a"
          (ok ['a'] "")
          (many_a "a");
        tcase "multiple_a_then_b"
          (ok ['a'; 'a'; 'a'] "b")
          (many_a "aaab");
        tcase "something_else_no_a"
          (ok [] "bbb")
          (many_a "bbb")
      ]);

      ("basic many1 tests",
      let tcase = quick_tcase char_list_parser_expect_equal in
      let many1_a = (many1 (charp 'a')) in [
        tcase "no_a"
          (error (ExpectationError "a") "")
          (many1_a "");
        tcase "one_a"
          (ok ['a'] "")
          (many1_a "a");
        tcase "multi_a"
          (ok ['a'; 'a'; 'a'; 'a'] "")
          (many1_a "aaaa");
        tcase "multi_a_then_b"
          (ok ['a'; 'a'; 'a'; 'a'] "bbb")
          (many1_a "aaaabbb");
        tcase "no_a_then_b"
          (error (ExpectationError "a") "bbb")
          (many1_a "bbb")
      ])
    ]
