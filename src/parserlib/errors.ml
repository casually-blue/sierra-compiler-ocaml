type parser_error = 
        | ExpectationError of string
        | EndOfInputError
        | ListError of parser_error * parser_error

let rec stringify_parser_error e = match e with
        | ExpectationError s -> "(Expected: " ^ s ^ ")"
        | EndOfInputError -> "Expected a character, got EOF"
        | ListError (e1, e2) -> (stringify_parser_error e1) ^ " or " ^ (stringify_parser_error e2)
