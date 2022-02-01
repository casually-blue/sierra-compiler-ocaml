open Parserlib.Combinators
open Parserlib.Basic

open Ast

(* parse a single-character operator *)
let parse_op op_char op = pmap_ok (charp op_char) (ok_ignore op)

(* parse the correct operators for each type of operation *)
let expr_oper = (parse_op '+' Plus) <|> (parse_op '-' Minus)
let term_oper = (parse_op '*' Times) <|> (parse_op '/' Divide)

let binary_op term op ctor = chainl1 (remove_whitespace term) (pmap_ok (remove_whitespace op) (ok_construct ctor))

