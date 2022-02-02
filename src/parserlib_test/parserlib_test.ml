open Alcotest

let () =
  run 
    "Parserlib Tests"
    [
      ("get_char tests", Get_char_tests.get_char_tests);
      ("match_char tests", Match_char_tests.match_char_tests);
      ("antimatch_char tests", Antimatch_char_tests.antimatch_char_tests);
      ("eof tests", EOF_tests.eof_tests);
      ("charp tests", Charp_tests.charp_tests);
      ("basic many tests", Many_tests.many_tests);
      ("basic many1 tests", Many1_tests.many1_tests)
    ]
