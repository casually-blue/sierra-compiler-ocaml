open Errors

type 'parsed parser_result = ('parsed * string, parser_error * string) result
type 'a parser_f = string -> 'a parser_result

val error: parser_error -> string -> 'a parser_result
val ok: 'a -> string -> 'a parser_result
