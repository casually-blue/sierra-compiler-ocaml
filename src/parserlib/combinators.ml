open Errors
open Types

(* put a type constructor into a parse function *)
let ok_construct con r = ok (con r)

(* flatten a nested tuple and apply the given constructor to the values *)
let flatmap ff constr value = ok_construct constr (ff value)

(* ignore the returned result and substitute it with your own *)
let ok_ignore v _ = ok v

(* map two functions onto the results of a parser *)
let pmap 
  (p: 'a parser_f) 
  (if_ok: 'a -> string -> 'b parser_result) 
  (if_error: parser_error -> string -> 'b parser_result) 
  (input: string): 
    'b parser_result
= match (p input) with
        | Ok (result, rest) -> if_ok result rest
        | Error (e, input) -> if_error e input

(* carry the value through if it is an error and execute the function if it isnt *)
let pmap_ok 
  (p: 'a parser_f) 
  (if_ok: 'a -> string -> 'b parser_result): 
    'b parser_f 
= pmap p if_ok error

(* carry the value through if it isn't an error and execute if it is *)
let pmap_error 
  (p: 'a parser_f) 
  (if_error: parser_error -> string -> 'a parser_result): 
    'a parser_f 
= pmap p ok if_error

(* get a char from the string or error if at end *)
let get_char s = (match (String.length s) with
  | 0 -> error EndOfInputError s
  | _ -> ok (String.get s 0) (String.sub s 1 ((String.length s) - 1)))

(* check if a character from the input matches a predicate *)
let match_char f e s = pmap get_char
  (fun r rest -> match (f r) with
    | true -> ok r rest
    | false -> error e s)

  (fun _ inp -> error e inp) s

(* match any char that doesn't fit a predicate *)
let antimatch_char f e s = pmap_ok get_char
  (fun r rest -> match (f r) with
    | false -> ok r rest
    | true -> error e s) s

let eof = pmap get_char
        (fun _ input -> error (ExpectationError "end of input") input)
        (fun _ inp -> ok () inp)


(* parse a single specific character *)
let charp c = match_char ((==) c) (ExpectationError (String.make 1 c))

(* repeat a given parser at least once *)
let rec many1 p = pmap_ok p 
  (fun r rest -> ( 
    pmap (many1 p) 
      (fun results rest -> Ok (r :: results, rest))
      (fun _ input -> Ok (r :: [], input)) 
      rest
  ))


(* repeat a given parser *)
let many p s = pmap_error (many1 p) (fun e _ -> (ok_ignore []) e s) s

(* chain two parsers together *)
let (<+>) p1 p2 = pmap_ok p1 
        (fun r1 rest -> pmap_ok p2
                (fun r2 rest -> Ok ((r1,r2), rest)) rest)



(* execute one parser and if it fails execute the second on the same input *)
let (<|>) p1 p2 = 
  (fun input_text -> pmap_error p1
     (fun e1 rest1 -> (match (String.length input_text, String.length rest1) with 
        | (il, rl) when il == rl ->
          (pmap_error p2
            (fun e2 rest2 -> (match (String.length rest2) with
              | rl when il == rl -> (error (ListError (e1, e2)) rest1)
              | _ -> (error e2 rest2)
            )) input_text)
        | (_,_) -> (error e1 rest1)
        )) input_text)

let (<?>) (p1: 'a parser_f) (euse: parser_error) input =
  pmap_error p1
    (fun e rest -> (match (String.length input, String.length rest) with
      | (il, rl) when il == rl -> (error euse rest)
      | _ -> error e rest)) input
      

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
let maybe p s = pmap p
        (fun r rest -> Ok(Some r, rest))
        (fun _ _ -> Ok(None, s)) s
