type parser_error = 
        | ExpectationError of string
        | EndOfInputError
        | ListError of parser_error list

type 'parsed parser_f = string -> ('parsed * string, parser_error) result

let get_char (s: string): (char * string, parser_error) result = (match (String.length s) with
	| 0 -> Error EndOfInputError
	| _ -> Ok (String.get s 0, (String.sub s 1 ((String.length s) - 1))))

let match_char (f: char -> bool) (err: parser_error) (s: string) = match (get_char s) with
	| Ok (c, rest) -> (
		match (f c) with
		| true -> Ok (c, rest)
		| false -> Error err)
	| Error e -> Error e

let isdigit c = c >= '0' && c <= '9'
let isalpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let match_digit = match_char isdigit (ExpectationError "digit")

let match_alpha = match_char isalpha (ExpectationError "alphabetic character")

let rec one_of (ps: ('parsed parser_f) list) (s: string) =
        match ps with
                | [] -> Error (ListError [])
                | p :: prest -> (
                        match (p s) with 
                        | Error e -> (match (one_of prest s) with
                                | Error ListError errs -> Error (ListError (e :: errs))
                                | good -> good
                        )
                        | good -> good
                )


let match_alnum = one_of (match_digit :: match_alpha :: [])

let rec many1 (p: 'parsed parser_f) (s: string) =
        match (p s) with
                | Ok (result, rest) -> (
                        match (many1 p rest) with
                                | Ok (results, rest) -> Ok (result :: results, rest)
                                | Error _ -> Ok (result :: [], rest)
                )
                | Error error -> Error error

let many (p: 'parsed parser_f) (s: string) =
        match (many1 p s) with
                | Error _ -> Ok ([], s)
                | good -> good

let match_digits = many1 match_digit
let char_to_number c = (Char.code c) - (Char.code '0')
let list_to_number l = List.fold_left 
                                (fun x y -> (x * 10 + y )) 
                                0 
                                (List.map char_to_number l)

let parser_map (p: 'parsed parser_f) if_ok_func s = match (p s) with
        | Ok (result, rest) -> Ok (if_ok_func result, rest)
        | Error error -> Error error

let is_whitespace c = (c == ' ' || c == '\t' || c == '\n')
let whitespace = many (match_char is_whitespace (ExpectationError "whitespace"))


let (<+>) p1 p2 s = match (p1 s) with
        | Ok (result, rest) -> (match (p2 rest) with
                | Ok (r2, rest) -> Ok ((result, r2), rest)
                | Error error -> Error error)
        | Error error -> Error error


let (<|>) p1 p2 s = match (p1 s) with
        | Error e1 -> (match (p2 s) with
                | Error e2 -> Error (ListError [e1; e2])
                | good -> good
        )
        | good -> good

let remove_whitespace p = parser_map (whitespace <+> p) (fun (_, r) -> r)
let integer = parser_map match_digits (fun r -> (list_to_number r))
let identifier =  parser_map (match_alpha <+> (many match_alnum)) (fun (first, rest) -> (String.make 1 first) ^ (String.of_seq (List.to_seq rest)))
