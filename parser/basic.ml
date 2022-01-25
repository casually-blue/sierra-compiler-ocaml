open Combinators

(* boolean functions to check for character set membership *)
let isdigit c = c >= '0' && c <= '9'
let isalpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_whitespace c = (c == ' ' || c == '\t' || c == '\n')

(* check for a single digit *)
let match_digit = match_char isdigit (ExpectationError "digit")
(* match at least one digit *)
let match_digits = many1 match_digit

(* match a single character *)
let match_alpha = match_char isalpha (ExpectationError "alphabetic character")
(* match either a digit or a alphabetic character *)
let match_alnum = match_digit <|> match_alpha
let match_alnum_ = match_digit <|> match_alpha <|> (charp '_')


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


