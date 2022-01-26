open Errors

(* a parser result of either a value and the rest of the input or a parser error *)
type 'parsed parser_result = ('parsed * string, parser_error * string) result
type 'parsed parser_f = string -> 'parsed parser_result

(* type constructors for parser results *)
let error e input : 'parsed parser_result = Error (e, input)
let ok r rest: 'parsed parser_result = Ok (r, rest)
