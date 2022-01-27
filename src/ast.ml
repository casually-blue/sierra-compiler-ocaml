
type op = 
        | Plus
        | Minus
        | Times
        | Divide

type expr = 
        | Number of int
        | Binary of op * expr * expr
        | ExprList of expr list
        | Function of string * expr 
        | Import of string
        | Binding of string * expr

let number n = Number n
let binary op left right = Binary (op, left, right)
let expr_list e = ExprList e
let function_c name e = Function (name,e)
let binding s exp = Binding (s,exp)
let import s = Import s

let func name block  = Function (name, block)

let op_to_string o = match o with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Divide -> "/"

let rec expression_to_string expr = (match expr with
        | Number n -> string_of_int n
        | Binary (o, l, r) -> "(" ^ (expression_to_string l) ^ (op_to_string o) ^ (expression_to_string r) ^ ")"
        | ExprList (expr :: rest) -> (expression_to_string expr) ^ "\n" ^ 
                (expression_to_string (expr_list rest))
        | ExprList [] -> ""
        | Binding (name,e) -> name ^ " = " ^ (expression_to_string e)
        | Import s -> "import: " ^ s 
        | Function (name, expr) -> "Function: " ^ name ^ " -> \n" ^ (expression_to_string expr))

