open Parserlib.Combinators
open Parserlib.Types

open Scaffold

let tcase = quick_tcase char_list_parser_expect_equal
let many_a = (many (charp 'a')) 

let many_tests = [
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
]

