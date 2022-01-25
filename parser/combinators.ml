type parser_error = 
        | ExpectationError of string
        | EndOfInputError
        | ListError of parser_error list

type 'parsed parser_result = ('parsed * string, parser_error) result
type 'parsed parser_f = string -> 'parsed parser_result

let pmap (p: 'parsed parser_f) (left: 'parsed -> 'string -> 'b parser_result) (right: 'e -> 'string -> 'f parser_result) (s: string) = match (p s) with
        | Ok (result, rest) -> left result rest
        | Error e -> right e s

let error e (_: string) = Error e
let ok (res: 'a) (rest: string): 'a parser_result = Ok (res, rest)

let pmap_ok p (left: 'a -> string -> 'parsed) = (pmap p left error)
let pmap_error p right = (pmap p ok right)

let get_char (s: string): (char * string, parser_error) result = (match (String.length s) with
        | 0 -> Error EndOfInputError
        | _ -> Ok (String.get s 0, (String.sub s 1 ((String.length s) - 1))))

let match_char (f: char -> bool) (err: parser_error) (s: string) = match (get_char s) with
        | Ok (c, rest) -> (
                match (f c) with
                | true -> Ok (c, rest)
                | false -> Error err)
        | Error e -> Error e

let charp c = match_char (fun ch -> c == ch) (ExpectationError (String.make 1 c))

let isdigit c = c >= '0' && c <= '9'
let isalpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let match_digit = match_char isdigit (ExpectationError "digit")

let match_alpha = match_char isalpha (ExpectationError "alphabetic character")

let rec one_of (ps: ('parsed parser_f) list) (s: string) =
        match ps with
                | [] -> Error (ListError [])
                | p :: prest -> (match (p s) with 
                        | Error e -> (match (one_of prest s) with
                                | Error ListError errs -> Error (ListError (e :: errs))
                                | good -> good)
                        | good -> good)

let match_alnum = one_of (match_digit :: match_alpha :: [])
let rec many1 (p: 'parsed parser_f) = pmap_ok p 
        (fun r rest -> ( pmap (many1 p) 
                (fun results rest -> Ok (r :: results, rest))
                (fun _ rest -> Ok (r :: [], rest)) 
                rest))


let many (p: 'parsed parser_f) = pmap_error (many1 p) (fun _ input -> Ok ([], input))

let match_digits = many1 match_digit
let char_to_number c = (Char.code c) - (Char.code '0')
let list_to_number l = List.fold_left 
                                (fun x y -> (x * 10 + y )) 
                                0 
                                (List.map char_to_number l)

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
                | good -> good)
        | good -> good

let remove_whitespace p = pmap_ok
        (whitespace <+> p) 
        (fun (_, result) rest -> Ok(result, rest)) 

let integer = pmap_ok
        match_digits 
        (fun r rest -> Ok(list_to_number r, rest)) 

let identifier = pmap_ok
        (match_alpha <+> (many match_alnum))
        (fun (first,rest) input -> Ok ((String.make 1 first) ^ (String.of_seq (List.to_seq rest)), input))

let keyword k = pmap_ok
        identifier
        (fun ident rest -> (match (String.equal k ident) with
                                | true -> Ok (k, rest)
                                | false -> Error (ExpectationError k)))

let rec sepBy sep p s = match (p s) with
        | Ok (result, rest) -> (match (sep rest) with
                | Ok (_, rest) -> (match (sepBy sep p rest) with
                        | Ok (results, rest) -> Ok (result :: results, rest)
                        | Error e -> Error e)
                | Error _ -> Ok (result :: [], rest))
        | Error e -> Error e

let rec chainl1 (p: 'a parser_f) (op: ('a -> 'a -> 'a) parser_f) (s: string) = match (p s) with
        | Ok (left, rest) -> (chain_rest p op left rest)
        | Error e -> Error e
and chain_rest p op a s = match (op s) with
        | Ok (oper, rest) -> (match (p rest) with
                        | Ok (right, rest) -> chain_rest p op (oper a right) rest
                        | Error e -> Error e)
        | Error _ -> Ok (a, s)

let rec chainr1 (p: 'a parser_f) (op: ('a -> 'a -> 'a) parser_f) (s: string) = match (p s) with
        | Ok (left, rest) -> (match (op rest) with
                | Ok (oper, rest) -> (match (chainr1 p op rest) with
                        | Ok (right, rest) -> Ok (oper left right, rest)
                        | Error e -> Error e)
                | Error _ -> Ok (left, rest))
        | Error e -> Error e

