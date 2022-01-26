open Combinators
open Errors

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


let whitespace = many (match_char is_whitespace (ExpectationError "whitespace"))

(* execute a parser after ignoring whitespace *)
let remove_whitespace p = pmap_ok
        (whitespace <+> p) 
        (fun (_, result) rest -> Ok(result, rest)) 

let (<-+>) p1 p2 = p1 <+> (remove_whitespace p2)

(* convert a char representation of a digit to the digit itself *)
let char_to_number c = (Char.code c) - (Char.code '0')

(* fold a list of numbers into a single number *)
let char_list_to_number l = List.fold_left 
                                (fun x y -> (x * 10 + y )) 
                                0 
                                (List.map char_to_number l)


(* match a positive integer *)
let integer = pmap_ok
        match_digits 
        (ok_construct char_list_to_number)

(* match an identifier that contains aphanumeric characters and possibly underscores *)
let identifier = pmap
        ((match_alpha <|> (charp '_')) <+> (many (match_alnum <|> (charp '_'))))
        (ok_construct (
                fun (first, rest) -> ((String.make 1 first) ^ (String.of_seq (List.to_seq rest)))))
        (fun _ input -> Error (ExpectationError "identifier", input))

(* match a keyword *)
let keyword k = pmap
        identifier
        (fun ident rest -> (match (String.equal k ident) with
                                | true -> Ok(k, rest)
                                | false -> Error((ExpectationError ("keyword: " ^ k)), rest)))
        (fun _ input -> Error ((ExpectationError ("keyword: " ^ k)), input))

