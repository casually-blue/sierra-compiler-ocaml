type op = 
  | Plus
  | Minus
  | Times
  | Divide

type qualified_id = 
  | Base of string
  | Wildcard
  | Qualified of qualified_id * qualified_id 

let base s = Base s
let wildcard = Wildcard
let qualified_id q s = Qualified (q, s)

type expr = 
  | Number of int
  | Binary of op * expr * expr
  | ExprList of expr list
  | Function of string * expr 
  | Import of qualified_id
  | Binding of string * expr
  | FnCall of string


let number n = Number n
let binary op left right = Binary (op, left, right)
let expr_list e = ExprList e
let function_c name e = Function (name,e)
let binding s exp = Binding (s,exp)
let import id = Import id
let fncall name = FnCall name

let func name block  = Function (name, block)

let op_to_string o = match o with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"

let rec qualified_id_to_string id = match id with
  | Wildcard -> "*"
  | Base s -> s
  | Qualified (q,s) -> (qualified_id_to_string q) ^ "/" ^ (qualified_id_to_string s)

let rec expression_to_string expr = match expr with
  | Number n -> string_of_int n
  | Binary (o, l, r) -> "(" ^ (expression_to_string l) ^ (op_to_string o) ^ (expression_to_string r) ^ ")"
  | ExprList (expr :: rest) -> (expression_to_string expr) ^ "\n" ^ (expression_to_string (expr_list rest))
  | ExprList [] -> ""
  | Binding (name,e) -> name ^ " = " ^ (expression_to_string e)
  | Import id -> "import: " ^ (qualified_id_to_string id)
  | Function (name, expr) -> "Function: " ^ name ^ " -> \n" ^ (expression_to_string expr)
  | FnCall name -> "calling: " ^ name
