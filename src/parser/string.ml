open Parserlib.Types
open Parserlib.Combinators
open Parserlib.Tuple

open Ast

(* parse a string escape *)
let escaped_char = pmap_ok ((charp '\\') <+> get_char )
  (fun (_,c) rest -> (match c with
    | '"' -> ok '"' rest
    | 'n' -> ok '\n' rest
    | 't' -> ok '\t' rest
    | '\\' -> ok '\\' rest
    | a -> error (ExpectationError ("escapable character not " ^ (Stdlib.String.make 1 a))) rest
  ))

(* parse a single character or escaped character of a string literal *)
let string_char = (escaped_char <|> 
  (antimatch_char (fun c -> c == '"' || c == '\\') (ExpectationError "not quote")))

let string_of_list l = Stdlib.String.of_seq (List.to_seq l)

(* parse a string literal*)
let string_p = (pmap_ok ((charp '"') <+> (many string_char) <+> (charp '"'))
  (flatmap flatten3 (fun (_,scs,_) -> string (string_of_list scs))) <?> (ExpectationError "String literal")

