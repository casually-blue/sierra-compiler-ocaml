open Errors
open Types

(* put a type constructor into a parse function *)
let ok_construct con r rest = Ok ((con r), rest)

(* ignore the returned result and substitute it with your own *)
let ok_ignore v _ rest = Ok (v, rest)

(* map two functions onto the results of a parser *)
let pmap (p: 'parsed parser_f) left right s = match (p s) with
        | Ok (result, rest) -> left result rest
        | Error (e, _) -> right e s


(* carry the value through if it is an error and execute the function if it isnt *)
let pmap_ok (p: 'parsed parser_f) (left: 'parsed -> string -> 'b parser_result)  = (pmap p left error)
(* carry the value through if it isn't an error and execute if it is *)
let pmap_error (p: 'parsed parser_f) (right: parser_error -> string -> 'b parser_result)  = (pmap p ok right)

(* get a char from the string or error if at end *)
let get_char s = (match (String.length s) with
        | 0 -> Error (EndOfInputError, s)
        | _ -> Ok (String.get s 0, (String.sub s 1 ((String.length s) - 1))))

(* check if a character from the input matches a predicate *)
let match_char f e = pmap_ok get_char
        (fun r rest -> match (f r) with
                | true -> Ok (r, rest)
                | false -> Error (e, rest))

let eof = pmap get_char
        (fun _ input -> Error (ExpectationError "end of input", input))
        (fun _ input -> Ok ((), input))


(* parse a single specific character *)
let charp c = match_char (fun ch -> c == ch) (ExpectationError (String.make 1 c))

(* repeat a given parser at least once *)
let rec many1 (p: 'parsed parser_f) = pmap_ok p 
        (fun r rest -> ( pmap (many1 p) 
                (fun results rest -> Ok (r :: results, rest))
                (fun _ rest -> Ok (r :: [], rest)) 
                rest))


(* repeat a given parser *)
let many (p: 'parsed parser_f): ('parsed list) parser_f  = pmap_error (many1 p) (ok_ignore [])

(* chain two parsers together *)
let (<+>) p1 p2 = pmap_ok p1 
        (fun r1 rest -> pmap_ok p2
                (fun r2 rest -> Ok ((r1,r2), rest)) rest)

(* execute one parser and if it fails execute the second on the same input *)
let (<|>) p1 p2 = pmap_error p1
        (fun e1 input -> pmap_error p2
                ((fun e2 rest -> match (e1, e2) with
                        | (ExpectationError e1, ExpectationError e2) -> error (ExpectationError (e1 ^ " or " ^ e2)) rest
                        | (ExpectationError e1, _) -> error (ExpectationError e1) rest
                        | (_, ExpectationError e2) -> error (ExpectationError e2) rest
                        | (e1, e2) -> error (ListError (e1, e2)) rest))
                input)

(* execute a parser repeatedly with another parser in between each instance *)
let rec sepBy sep p = pmap_ok p
        (fun result rest -> pmap sep
                (fun _ rest -> pmap_ok (sepBy sep p)
                        (fun results rest -> ok (result :: results) rest) rest)
                (fun _ input -> ok (result :: []) input) rest)

(* left-associatively run a parser with an op *)
let rec chainl1 p op = pmap_ok p (fun r rest -> chain_rest p op r rest)
and chain_rest p op a = pmap op
        (fun oper rest -> pmap_ok p 
                (fun right rest -> chain_rest p op (oper a right) rest) rest)
        (fun _ input -> ok a input)

(* right-associatively run a parser with an op *)
let rec chainr1 p op = pmap_ok p
        (fun left rest -> pmap op
                (fun oper rest -> pmap_ok (chainr1 p op)
                        (fun right rest -> Ok (oper left right, rest)) rest)
                (fun _ input -> Ok (left, input)) rest)

(* maybe apply a parser to input *)
let maybe p = pmap p
        (fun r rest -> Ok(Some r, rest))
        (fun _ input -> Ok(None, input))