open Parserlib.Combinators
open Parserlib.Types

open Scaffold

let tcase = quick_tcase char_parser_expect_equal
let match_a = (charp 'a') 

let charp_tests = [ 
  tcase "just_a"
    (ok 'a' "")
    (match_a "a");
  tcase "a_with_rest"
    (ok 'a' "b")
    (match_a "ab")
]

