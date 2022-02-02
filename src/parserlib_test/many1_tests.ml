open Parserlib.Combinators
open Parserlib.Errors
open Parserlib.Types

open Scaffold

let tcase = quick_tcase char_list_parser_expect_equal
let many1_a = (many1 (charp 'a')) 

let many1_tests = [
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
]
