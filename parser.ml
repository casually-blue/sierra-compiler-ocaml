open Parserlib.Combinators
open Parserlib.Basic

open Ast

(* parse number *)
let number = pmap_ok integer (ok_construct number)

(* parse a single-character operator *)
let parse_op op_char op = pmap_ok (charp op_char) (ok_ignore op)

(* parse the correct operators for each type of operation *)
let expr_oper = (parse_op '+' Plus) <|> (parse_op '-' Minus)
let term_oper = (parse_op '*' Times) <|> (parse_op '/' Divide)

let binary_op term op ctor = chainl1 (remove_whitespace term) (pmap_ok (remove_whitespace op) (ok_construct ctor))

(* left-associative parse expressions and terms *)
let term = binary_op number term_oper binary
let expr = binary_op term expr_oper binary

let binding = pmap_ok
                ((keyword "let") <-+> identifier <-+> (charp '=') <-+> expr)
                (fun (((_, name),_), exp) rest -> Ok (binding name exp, rest))

let bare_expression = pmap_ok expr (ok_construct stmt_expr)

let stmt = pmap_ok 
        ((binding <|> bare_expression) <-+> (charp ';'))
        (fun (exp, _) rest -> Ok (exp, rest))

let funcp = pmap_ok
        ((keyword "function") <-+> identifier <-+> (charp '(') <-+> (charp ')') <-+> (charp '{') <-+> (many stmt) <-+> (charp '}'))
        (fun ((((((_, name), _), _), _), stmts), _) rest -> Ok((func name (block stmts)), rest))

let programp = pmap_ok (remove_whitespace (funcp <-+> eof))
        (fun (f, ()) rest -> Ok(f, rest))
