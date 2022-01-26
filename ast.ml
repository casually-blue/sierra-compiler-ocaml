
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

