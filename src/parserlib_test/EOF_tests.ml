open Parserlib.Combinators
open Parserlib.Errors
open Parserlib.Types

open Scaffold


let tcase = quick_tcase unit_parser_expect_equal

let eof_tests = [
  tcase "eof"
    (ok () "")
    (eof "");
  tcase "eof_error"
    (error (ExpectationError "end of input") "a")
    (eof "a")
]
