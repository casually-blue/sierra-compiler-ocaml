open Parserlib.Combinators
open Parserlib.Types
open Parserlib.Errors

open Scaffold

let tcase = quick_tcase char_parser_expect_equal 

let get_char_tests = [
  tcase "single_char"
    (ok 'a' "b")
    (get_char "ab");
  tcase "no_char"
    (error (EndOfInputError) "")
    (get_char "");
  tcase "only_one_char"
    (ok 'a' "") 
    (get_char "a")
]
