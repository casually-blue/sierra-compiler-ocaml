open Parserlib.Combinators
open Parserlib.Basic

open Ast

(* parse number *)
let number_p = pmap_ok integer (ok_construct number)

