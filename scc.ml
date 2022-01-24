open Src.Parser_combinators

let whitespace_and_num = skip_whitespace <+> integer 
let ident_ws_num = skip_whitespace <+> identifier <+> skip_whitespace <+> integer 
