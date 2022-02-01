open Parserlib.Combinators
open Parserlib.Basic
open Parserlib.Tuple

open Expression

(* a program is just an expression followed by the end of input *)
let program = 
  (pmap_ok
    (remove_whitespace (expression_list_p) <-+> eof)
    (flatmap flatten2 (fun (ex, _) -> Some ex))) 
  <|> (pmap_ok 
    (remove_whitespace eof) 
    (ok_ignore None))
