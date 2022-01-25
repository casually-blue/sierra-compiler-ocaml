open Parser.Combinators
type op = 
        | Plus
        | Minus
        | Times
        | Divide

type expr = 
        | Number of int
        | Binary of op * expr * expr

let binary op left right = Binary (op, left, right)

let number = pmap_ok (remove_whitespace integer) (fun i rest -> Ok(Number i, rest))

let ignore_ws_p p = remove_whitespace p
let parse_op opc op = pmap_ok (ignore_ws_p (charp opc)) (fun _ rest -> Ok(op, rest))

let plus = parse_op '+' Plus
let minus = parse_op '-' Minus
let expr_oper = plus <|> minus

let times = parse_op '*' Times
let divide = parse_op '/' Divide
let term_oper = times <|> divide

let term = chainl1 number (pmap_ok term_oper (fun res rest -> ok (binary res) rest))
let expr = chainl1 term (pmap_ok expr_oper (fun res rest -> ok (binary res) rest))

let rec eval (expression: expr): int = match expression with
        | Number a -> a
        | Binary (op,left,right) -> (match op with
                | Plus -> (+)
                | Minus -> (-)
                | Times -> fun a b -> (a * b)
                | Divide -> (/)) (eval left) (eval right)

let () = while true do
        print_string ">> ";
        let line = read_line() in
        print_string "Result: ";
        match (expr line) with
                | Ok (e, _) -> print_int (eval e)
                | _ -> print_endline "Error parse failed";
done
