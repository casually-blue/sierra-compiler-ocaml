open Parserlib.Types
open Parserlib.Errors
open Parserlib.Combinators

open Alcotest
open Scaffold 

let () =
  run "Parserlib Tests"
    [
      ("get_char tests", Get_char_tests.get_char_tests);
      ("match_char tests", Match_char_tests.match_char_tests);

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
