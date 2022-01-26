open Parser.Combinators
open Parser.Basic

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
let () = let exiting = ref false in while (not !exiting) do
        (* prompt for a line *)
        print_string ">> ";
        let line = (try Some(read_line()) with 
                End_of_file -> None
        ) in

        (* parse the line *)
        match line with
        | Some line -> (
                (* Parse the line *)
                match ((remove_whitespace (stmt <-+> eof)) line) with
                (* Eval, throwing away the rest of input and the eof which got parsed *)
                | Ok ((s, ()), _) -> (match s with
                        | Binding (name, exp) ->
                                        print_string ("Result: " ^ name ^ " = ") ; print_int (eval exp)
                        | Expression exp -> print_string "Result: " ; print_int (eval exp); print_endline ""
                        )
                (* if there is an error just print it out *)
                        | Error (err, input) -> print_endline ("Error parse failed:" ^ (stringify_parser_error err) ^ " (" ^ input ^ ")");)
        | None -> exiting := true
done
