open Combinators
open Errors
open Types

(* boolean functions to check for character set membership *)
let isdigit c = c >= '0' && c <= '9'
let isalpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_whitespace (c: char) = (c == ' ' || c == '\t' || c == '\n')

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
        (fun (_, result) rest -> (ok result rest))

let (<-+>) p1 p2 = p1 <+> (remove_whitespace p2)

(* convert a char representation of a digit to the digit itself *)
let char_to_number c = (Char.code c) - (Char.code '0')

(* fold a list of numbers into a single number *)
let char_list_to_number l = List.fold_left 
                                (fun x -> ((+) (x * 10)))
                                0 
                                (List.map char_to_number l)


(* match a positive integer *)
let integer = (pmap_ok
        match_digits 
        (ok_construct char_list_to_number)) <?> (ExpectationError "Integer")

(* match an identifier that contains aphanumeric characters and possibly underscores *)
let identifier = (pmap_ok
        (match_alpha <+> (many match_alnum))
        (ok_construct (
                fun (first, rest) -> ((String.make 1 first) ^ (String.of_seq (List.to_seq rest)))))) <?> (ExpectationError "Identifier")

(* match a keyword *)
let keyword k s = pmap
        identifier
        (fun ident rest -> (match (String.equal k ident) with
                                | true -> ok k rest
                                | false -> error (ExpectationError ("keyword: " ^ k)) s))
        (fun _ input -> error (ExpectationError ("keyword: " ^ k)) input) s

