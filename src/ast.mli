type op = 
  | Plus
  | Minus
  | Times
  | Divide

type qualified_id

val base: string -> qualified_id
val wildcard: qualified_id
val qualified_id: qualified_id -> qualified_id -> qualified_id

type expr

val number: int -> expr
val binary: op -> expr -> expr -> expr
val expr_list: expr list -> expr
val function_c: string -> expr -> expr
val binding: string -> expr -> expr
val import: qualified_id -> expr
val func: string -> expr -> expr
val fncall: string -> expr
val string: string -> expr

val expression_to_string: expr -> string
