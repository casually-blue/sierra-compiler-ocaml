open Parser.Combinators
open Parser.Basic

open Batteries

type op = 
        | Plus
        | Minus
        | Times
        | Divide

type expr = 
        | Number of int
        | Binary of op * expr * expr

let binary op left right = Binary (op, left, right)
let number n = Number n

type stmt = 
        | Expression of expr
        | Binding of string * expr

let stmt_expr exp = Expression exp
let binding s exp = Binding (s,exp)

(* parse number *)
let number = pmap_ok (remove_whitespace integer) (ok_construct number)

(* parse a single-character operator *)
let parse_op op_char op = pmap_ok (remove_whitespace (charp op_char)) (ok_ignore op)

(* parse the correct operators for each type of operation *)
let expr_oper = (parse_op '+' Plus) <|> (parse_op '-' Minus)
let term_oper = (parse_op '*' Times) <|> (parse_op '/' Divide)

(* left-associative parse expressions and terms *)
let term = chainl1 number (pmap_ok term_oper (ok_construct binary))
let expr = chainl1 term (pmap_ok expr_oper (ok_construct binary))

let stmt = 
        (pmap_ok 
                (
                        (remove_whitespace (keyword "let")) <+> 
                        (remove_whitespace identifier) <+>
                        (remove_whitespace (charp '=')) <+>
                        (remove_whitespace expr) <+>
                        (remove_whitespace (charp ';')))
                (fun ((((_, name),_), exp), _) rest -> Ok (binding name exp, rest))) <|>
        (pmap_ok
                ((remove_whitespace expr) <+> (remove_whitespace (charp ';')))
                (fun (exp, _) rest -> Ok (stmt_expr exp, rest)))

(* recursively evaluate expressions *)
let rec eval (expression: expr): int = match expression with
        (* just give the value *)
        | Number a -> a
        (* get a function to apply to the left and right values *)
        | Binary (op,left,right) -> (match op with
                | Plus -> (+)
                | Minus -> (-)
                | Times -> fun a b -> (a * b)
                | Divide -> (/)) 
        (* recurse *)
        (eval left) (eval right)

(* REPL *)
let () = while true do
        (* prompt for a line *)
        print_string ">> ";
        let line = read_line() in

        (* print result *)
        print_string "Result: ";

        (* Parse the line *)
        match (stmt line) with
                (* Eval *)
                | Ok (s, _) -> (match s with
                        | Binding (name, exp) ->
                               print_string (name ^ " = ") ; print_int (eval exp)
                        | Expression exp -> print_int (eval exp); print_endline ""
                )
                | Error err -> print_endline ("Error parse failed:" ^ (stringify_parser_error err));
done
