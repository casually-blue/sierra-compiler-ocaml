open Src.Parser_combinators
type op = 
        | Plus
        | Minus
        | Times
        | Divide

type expr = 
        | Number of int
        | Binary of op * expr * expr

let number = remove_whitespace integer

let plus = (parser_map (remove_whitespace (charp '+')) (fun _ -> Plus))
let minus = (parser_map (remove_whitespace (charp '-')) (fun _ -> Minus))
let eoper = plus <|> minus
let times = (parser_map (remove_whitespace (charp '*')) (fun _ -> Times))
let divide = (parser_map (remove_whitespace (charp '/')) (fun _ -> Divide))
let toper = times <|> divide


let term = sepBy toper number
let expr = sepBy eoper term 
