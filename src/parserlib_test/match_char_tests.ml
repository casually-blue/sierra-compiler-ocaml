open Parserlib.Combinators
open Parserlib.Types
open Parserlib.Errors


open Scaffold

let tcase = quick_tcase char_parser_expect_equal
let match_a = (match_char ((=) 'a') (ExpectationError "a"))

let match_char_tests = [
  tcase "match_a" 
    (ok 'a' "b")
    (match_a "ab");
  tcase "error_a"
    (error (ExpectationError "a") "bb")
    (match_a "bb")
]
