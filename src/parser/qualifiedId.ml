open Parserlib.Combinators
open Parserlib.Basic

open Ast

let qualified_id_p s = chainl1 
  ((pmap_ok identifier (ok_construct base)) <|> (pmap_ok (charp '*') (ok_ignore wildcard)))
  (pmap_ok (charp '.') (fun _ rest -> Parserlib.Types.Ok((fun l r -> (qualified_id l r)), rest))) s

