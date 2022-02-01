open Parserlib.Combinators
open Parserlib.Basic
open Parserlib.Errors
open Parserlib.Tuple


open String
open BinOp
open QualifiedId
open Number

open Ast

let rec expression_p = function s -> (( string_p <|> expr_binary <|> function_p <|> import_p <|> binding_p <|> fncall_p ) <?> (ExpectationError "Expression")) s

(* parse expressions separated by semicolons *)
and expression_list_p = function s -> (pmap_ok
  (sepBy 
    (remove_whitespace (charp ';')) 
    (remove_whitespace expression_p))
  (ok_construct expr_list)) s

(* parse a let binding of the form "let x = expression" *)
and binding_p = function s -> ((pmap_ok
  ((keyword "let") <-+> identifier <-+> (charp '=') <-+> expression_p)
  (flatmap flatten4 (fun (_,name,_,exp) -> (binding name exp)))) 
  <?> (ExpectationError "Let binding")) s

(* parse a function call of the form "name()" *)
and fncall_p = function s -> ((pmap_ok
  (identifier <-+> (charp '(') <-+> (charp ')'))
  (flatmap flatten3 (fun (name,_,_) -> (fncall name)))) <?> (ExpectationError "Function call")) s

(* parse an import of the form "import name" *)
and import_p = function s -> ((pmap_ok
  ((keyword "import") <-+> qualified_id_p)
  (flatmap flatten2 (fun (_, qid) -> (import qid)))) <?> (ExpectationError "Import expression")) s

(* parse a function of the form "fun name () { expression }" *)
and function_p = function s -> ((pmap_ok
  ((keyword "fun") <-+> (charp '(') <-+> (charp ')') <-+> (charp '{') <-+> expression_p <-+> (charp '}'))
  (flatmap flatten6 (fun (_,_,_,_,expr,_) -> (func expr)))) <?> (ExpectationError "Function definition")) s

(* left-associative parse arithmentically expressions *)
and expr_binary = function s -> binary_op (binary_op number_p term_oper binary) expr_oper binary s

