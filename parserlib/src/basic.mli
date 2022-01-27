open Types

(* match a number *)
val integer: int parser_f

(* match any string *)
val identifier: string parser_f

(* match a specific string *)
val keyword: string -> string parser_f

(* parse some whitespace *)
val whitespace: (char list) parser_f

(* remove whitespace and then match a parser *)
val remove_whitespace: 'a parser_f -> 'a parser_f

(* match a parser, ignore whitespace and then match another parser *)
val (<-+>): 'a parser_f -> 'b parser_f -> ('a * 'b) parser_f
