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

(* parse expressions separated by semicolons *)
let rec expression_p s = (pmap_ok
                (
                        (* parse one of the types of expressions *)
                        ( expr_binary <|> function_p <|> import_p <|> binding_p ) 
                        (* if we have a semicolon recurse *)
                        <-+> (maybe ((charp ';') <-+> expression_p))
                )
                (fun (e,m_exp) rest -> (match m_exp with
                        | Some((_, eml)) -> (match eml with
                                | ExprList el -> Ok (expr_list (e :: el), rest)
                                | e2 -> Ok(expr_list (e :: e2 :: []), rest))
                        | None -> Ok (e, rest)))) s

(* parse a let binding of the form "let x = expression" *)
and binding_p s = pmap_ok
                        ((keyword "let") <-+> identifier <-+> (charp '=') <-+> expression_p)
                        (flatmap flatten4 (fun (_,name,_,exp) -> (binding name exp))) s

(* parse an import of the form "import name" *)
and import_p s = pmap_ok
                ((keyword "import") <-+> identifier)
                (flatmap flatten2 (fun (_, lib) -> (import lib))) s

(* parse a function of the form "fun name () { expression }" *)
and function_p s = pmap_ok
        ((keyword "fun") <-+> identifier <-+> (charp '(') <-+> (charp ')') <-+> (charp '{') <-+> expression_p <-+> (charp '}'))
        (flatmap flatten7 (fun (_,name,_,_,_,expr,_) -> (func name expr))) s

(* left-associative parse expressions *)
and expr_binary s = binary_op (binary_op number_p term_oper binary) expr_oper binary s

(* a program is just an expression followed by the end of input *)
let program = pmap_ok (remove_whitespace (expression_p <-+> eof))
        (flatmap flatten2 (fun (e, ()) -> e))
