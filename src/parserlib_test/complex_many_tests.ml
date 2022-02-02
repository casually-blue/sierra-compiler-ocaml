open Scaffold

open Parserlib.Combinators
open Parserlib.Basic
open Parserlib.Types

let string_list_parser_expect_equal = 
  expect_equal (parser_result_ttb (Format.pp_print_list Format.pp_print_string))

let number_list_parser_ee = 
  expect_equal (parser_result_ttb (Format.pp_print_list Format.pp_print_int))

let tcase = quick_tcase string_list_parser_expect_equal
let int_tcase = quick_tcase number_list_parser_ee

let many_ident_sws = (many (remove_whitespace identifier))
let many_num_sws = (many (remove_whitespace integer))

let complex_many_tests = [
  tcase "no_ident"
    (ok [] "")
    (many_ident_sws "");
  tcase "one_ident"
    (ok ["aaa"] "")
    (many_ident_sws "aaa");
  tcase "one_ident_ws"
    (ok ["aaa"] "")
    (many_ident_sws "   aaa");
  tcase "one_ident_ws_after"
    (ok ["aaa"] "  ")
    (many_ident_sws "   aaa  ");
  tcase "one_ident_ws_only_after"
    (ok ["aaa"] "  ")
    (many_ident_sws "aaa  ");
  tcase "idents_with_ws_between"
    (ok ["aaa"; "bbb"] "")
    (many_ident_sws "aaa  bbb");
  tcase "idents_with_ws_after"
    (ok ["aaa"; "bbb"] "   ")
    (many_ident_sws "aaa  bbb   ");
  tcase "idents_with_ws_before_and_after"
    (ok ["aaa"; "bbb"] "   ")
    (many_ident_sws "   aaa   bbb   ");

  int_tcase "no_number"
    (ok [] "")
    (many_num_sws "");
  int_tcase "one_number"
    (ok [22] "")
    (many_num_sws "22");
  int_tcase "one_number_ws"
    (ok [22] "  ")
    (many_num_sws "  22  ");
]
