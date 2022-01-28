open Types
open Errors

(* map a function over a parser's result *)
val ok_construct: ('a -> 'b) -> 'a -> 'b parser_f

(* map f(g(parser_result)) *)
val flatmap: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c parser_f

(* replace the result of a parser with a constant value *)
val ok_ignore: 'a -> 'b -> string -> 'a parser_result

(* map a function over the ok and error branches of a parser *)
val pmap: 'a parser_f -> ('a -> string -> 'b parser_result) -> (parser_error -> string -> 'b parser_result) -> 'b parser_f

(* map a function over only the ok branch of a parser *)
val pmap_ok: 'a parser_f -> ('a -> string -> 'b parser_result) -> 'b parser_f

(* map a function over only the error branch of a parser *)
val pmap_error: 'a parser_f -> (parser_error -> string -> 'b parser_result) -> string -> 'b parser_result

(* check if a character satisfies a predicate *)
val match_char: (char -> bool) -> parser_error -> string -> char parser_result

(* return an error if the input is not at its end *)
val eof: string -> unit parser_result

(* parse a specific char *)
val charp: char -> char parser_f

(* match at least one instance of a parser *)
val many1: 'a parser_f -> ('a list) parser_f

(* match a parser at least 0 times *)
val many: 'a parser_f -> ('a list) parser_f

(* sequence parsers *)
val (<+>): 'a parser_f -> 'b parser_f -> ('a * 'b) parser_f

(* run one parser and then run another parser on the same input if it fails *)
val (<|>): 'a parser_f -> 'a parser_f -> 'a parser_f

(* match multiple occurences of a parser seperated by some other parser *)
val sepBy: 'a parser_f -> 'b parser_f -> ('b list) parser_f

(* left-associatively match a parser seperated by an operation *)
val chainl1: 'a parser_f -> ('a -> 'a -> 'a) parser_f -> 'a parser_f

(* right-associatively match a parser seperated by an operation *)
val chainr1: 'a parser_f -> ('a -> 'a -> 'a) parser_f -> 'a parser_f

(* optionally match a parser *)
val maybe: 'a parser_f -> ('a option) parser_f
