open Errors

type 'parsed parser_result = 
  | Ok of 'parsed * string
  | Error of parser_error * string
  [@@deriving show]

type 'a parser_f = string -> 'a parser_result

val error: parser_error -> string -> 'a parser_result
val ok: 'a -> string -> 'a parser_result
