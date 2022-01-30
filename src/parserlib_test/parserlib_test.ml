open Parserlib.Types
open Parserlib.Errors

module To_test = struct
  let get_char = Parserlib.Combinators.get_char
  let parser_result_eq pa pb = (pa = pb)

  let pprint_parser_result ppf = function
    | Ok (c,rest) -> Fmt.pf ppf "%c,%s" c rest
    | Error (_,rest) -> Fmt.pf ppf "%s" rest
  let presult_testable = Alcotest.testable pprint_parser_result parser_result_eq
end
  let test_onechar () = 
    Alcotest.(check To_test.presult_testable) "Same result" (ok 'a' "b") (To_test.get_char "ab")
  let test_nochar () = 
    Alcotest.(check To_test.presult_testable) "Error no value" (error (EndOfInputError) "") (To_test.get_char "")

let () =
  Alcotest.run "Parserlib Tests"
    [
      ("get_char tests",
      [
        Alcotest.test_case "test_single_char" `Quick test_onechar;
        Alcotest.test_case "test_no_char" `Quick test_nochar
      ])
    ]
