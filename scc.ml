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

type block = Block of (stmt list)
let block stmts = Block stmts

type func = Function of string * block
let func name block  = Function (name, block)

let stmt_expr exp = Expression exp
let binding s exp = Binding (s,exp)

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

let stmt_to_string stmt = match stmt with
        | Expression e -> string_of_int (eval e)
        | Binding (name,e) -> name ^ " = " ^ (string_of_int (eval e))

let block_to_string (Block stmts) = List.fold_left
                                (fun l r -> l ^ "\n\t" ^ (stmt_to_string r))
                                ""
                                stmts

let func_to_string (Function (name, block)) = "function " ^ name ^ "{{ " ^ (block_to_string block) ^ "\n}}\n "


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
        (fun ((((((_, name), _), _), _), stmts), _) rest -> ok (func name (block stmts)) rest)


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
                match ((remove_whitespace (funcp <-+> eof)) line) with
                (* Eval, throwing away the rest of input and the eof which got parsed *)
                | Ok ((f, ()), _) -> print_string (func_to_string f)                 
                (* if there is an error just print it out *)
                | Error (err, input) -> print_endline ("Error parse failed:" ^ (stringify_parser_error err) ^ " (" ^ input ^ ")");)
        | None -> exiting := true
done
