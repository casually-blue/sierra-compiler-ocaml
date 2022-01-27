open Parserlib.Combinators
open Parserlib.Basic
open Parserlib.Tuple

open Ast

(* parse number *)
let number_p = pmap_ok integer (ok_construct number)

(* parse a single-character operator *)
let parse_op op_char op = pmap_ok (charp op_char) (ok_ignore op)

(* parse the correct operators for each type of operation *)
let expr_oper = (parse_op '+' Plus) <|> (parse_op '-' Minus)
let term_oper = (parse_op '*' Times) <|> (parse_op '/' Divide)

let binary_op term op ctor = chainl1 (remove_whitespace term) (pmap_ok (remove_whitespace op) (ok_construct ctor))

(* left-associative parse expressions and terms *)
let term = binary_op number_p term_oper binary
let expr_binary = binary_op term expr_oper binary

let rec non_list_expression_p s = ( expr_binary <|> function_p <|> import_p <|> binding_p ) s
and expression_p s = (expr_list_p <|> non_list_expression_p) s
and binding_p s = pmap_ok
                ((keyword "let") <-+> identifier <-+> (charp '=') <-+> expression_p)
                (flatmap flatten4 (fun (_,name,_,exp) -> (binding name exp))) s
and import_p = pmap_ok
                ((keyword "import") <-+> identifier)
                (flatmap flatten2 (fun (_, lib) -> (import lib)))
and function_p s = pmap_ok
        ((keyword "fun") <-+> identifier <-+> (charp '(') <-+> (charp ')') <-+> (charp '{') <-+> expression_p <-+> (charp '}'))
        (flatmap flatten7 (fun (_,name,_,_,_,expr,_) -> (func name expr))) s
and expr_list_p s = (pmap_ok
                (non_list_expression_p <-+> (charp ';') <-+> expression_p)
                (flatmap flatten3 (fun (e,_,eml) -> (match eml with
                        | ExprList el -> (expr_list (e :: el))
                        | e2 -> (expr_list (e :: e2 :: [])))))) s

let program = pmap_ok (remove_whitespace (expression_p <-+> eof))
        (flatmap flatten2 (fun (e, ()) -> e))
