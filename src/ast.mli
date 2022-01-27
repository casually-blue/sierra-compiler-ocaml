type op = 
  | Plus
  | Minus
  | Times
  | Divide

type expr

val number: int -> expr
val binary: op -> expr -> expr -> expr
val expr_list: expr list -> expr
val function_c: string -> expr -> expr
val binding: string -> expr -> expr
val import: string -> expr
val func: string -> expr -> expr

val op_to_string: op -> string
val expression_to_string: expr -> string
