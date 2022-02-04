open Parserlib.Types
open Parserlib.Combinators
open Parserlib.Errors

open Ast

(* parse a string escape *)
let escaped_char = (function s -> (pmap_ok (charp '\\')
  (fun _ rest -> pmap_ok get_char 
    (fun c restc -> (match c with 
    | '"' -> ok '"' restc
    | 'n' -> ok '\n' restc
    | 't' -> ok '\t' restc
    | '\\' -> ok '\\' restc
    | _ -> error (ExpectationError "escapable character") rest)) rest)) s)
  <?> (ExpectationError "Escapable character")

(* parse a single character or escaped character of a string literal *)
let string_char = (escaped_char <|> 
  (antimatch_char (fun c -> c == '"' || c == '\\') (ExpectationError "not quote")))

let string_of_list l = Stdlib.String.of_seq (List.to_seq l)

let string_p = pmap_ok (charp '"')
  (fun _ rest ->
    match (many string_char rest) with
      | Ok (chrs, rest) -> 
          pmap_ok (charp '"')
            (fun _ rest -> ok (string (string_of_list chrs)) rest) 
            rest
      | Error (e, inp) -> print_endline inp; error e inp) 

