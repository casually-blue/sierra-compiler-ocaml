open Combinators

let isdigit c = c >= '0' && c <= '9'
let isalpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let match_digit = match_char isdigit (ExpectationError "digit")
let match_digits = many1 match_digit

let match_alpha = match_char isalpha (ExpectationError "alphabetic character")
let match_alnum = match_digit <|> match_alpha


let is_whitespace c = (c == ' ' || c == '\t' || c == '\n')
let whitespace = many (match_char is_whitespace (ExpectationError "whitespace"))
let remove_whitespace p = pmap_ok
        (whitespace <+> p) 
        (fun (_, result) rest -> Ok(result, rest)) 

let char_to_number c = (Char.code c) - (Char.code '0')
let list_to_number l = List.fold_left 
                                (fun x y -> (x * 10 + y )) 
                                0 
                                (List.map char_to_number l)

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


