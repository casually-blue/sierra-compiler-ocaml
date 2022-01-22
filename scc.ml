type parser_error = ExpectationError of string

type parser_f = string -> (char * string, parser_error) result

let get_char (s: string): (char * string, parser_error) result = (match (String.length s) with
	| 0 -> Error (ExpectationError "character")
	| _ -> Ok (String.get s 0, (String.sub s 1 ((String.length s) - 1))))

let match_char f err = fun s -> (
	match (get_char s) with
	| Ok (c, rest) -> (
		match (f c) with
		| true -> Ok (c, rest)
		| false -> Error err)
	| Error e -> Error e
)

let isdigit c = c >= '0' && c <= '9'
let isalpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let match_digit = match_char isdigit (ExpectationError "digit")

let match_alpha = match_char isalpha (ExpectationError "alphabetic character")

let either (a: parser_f) (b: parser_f) = fun s -> (
	match (a s) with
	| Ok (c, rest) -> Ok (c, rest)
	| Error (ExpectationError e1) -> (
		match (b s) with
		| Ok (c, rest) -> Ok (c, rest)
		| Error (ExpectationError e2) -> Error (ExpectationError (e1 ^ " or " ^ e2))
	)
)

let seq (ps : parser_f list) = List.length ps

let match_alnum = (either match_digit match_alpha)
