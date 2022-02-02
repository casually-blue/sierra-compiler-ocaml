open Parserlib.Types

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

