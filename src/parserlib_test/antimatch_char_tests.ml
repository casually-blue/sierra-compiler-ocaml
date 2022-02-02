open Parserlib.Combinators
open Parserlib.Types
open Parserlib.Errors


open Scaffold

let tcase = quick_tcase char_parser_expect_equal
let antimatch_b = (antimatch_char ((=) 'b') (ExpectationError "not b")) 

let antimatch_char_tests = [
  tcase "nomatch_a"
    (ok 'a' "b")
    (antimatch_b "ab");
  tcase "nomatch_a_error"
    (error (ExpectationError "not b") "bb")
    (antimatch_b "bb")
]
