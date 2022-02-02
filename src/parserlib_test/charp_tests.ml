open Parserlib.Combinators
open Parserlib.Types
open Parserlib.Errors

open Scaffold

let tcase = quick_tcase char_parser_expect_equal
let match_a = (charp 'a') 

let qtest = QCheck.Test.make ~count:1000
    ~name:"charp only correct match"
    QCheck.((pair char char))
    (fun (c,d) -> (if c=d then 
      (charp c) (String.make 1 d) = (ok c "")
    else 
      (charp c) (String.make 1 d) = (error (ExpectationError (String.make 1 c)) (String.make 1 d))));;

let qtest_more = QCheck.Test.make ~count:1000
  ~name:"charp then other"
  QCheck.(pair char string)
  (fun (c, s) -> 
  if ((String.length s)>=2) then (
    if (String.sub s 0 1) = (String.make 1 c) then
      (charp c) s = (ok c (String.sub s 1 ((String.length s)-1)))
    else
      (charp c) s = (error (ExpectationError (String.make 1 c)) s)) 
  else true);;

let charp_tests = [ 
  tcase "just_a"
    (ok 'a' "")
    (match_a "a");
  tcase "a_with_rest"
    (ok 'a' "b")
    (match_a "ab");
  (QCheck_alcotest.to_alcotest qtest);
  (QCheck_alcotest.to_alcotest qtest_more);
]

