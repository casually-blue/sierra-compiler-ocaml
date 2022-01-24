open Src.Parser_combinators
type op = 
        | Plus
        | Minus
        | Times
        | Divide

type expr = 
        | Number of int
        | Binary of op * expr * expr

let binary op left right = Binary (op, left, right)

let number = parser_map (remove_whitespace integer) (fun i -> Number i)

let plus = (parser_map (remove_whitespace (charp '+')) (fun _ -> Plus))
let minus = (parser_map (remove_whitespace (charp '-')) (fun _ -> Minus))
let eoper = plus <|> minus
let times = (parser_map (remove_whitespace (charp '*')) (fun _ -> Times))
let divide = (parser_map (remove_whitespace (charp '/')) (fun _ -> Divide))
let toper = times <|> divide

let term = chainl1 number (parser_map toper binary)
let expr = chainl1 term (parser_map eoper binary)
