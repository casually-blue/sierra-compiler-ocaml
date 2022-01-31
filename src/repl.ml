open Format
open Fmt

open Parserlib.Types

open Ast
open Parser

let prompt_line prompt = 
  let open Core.In_channel in
  fprintf stderr "%s%!" prompt;
  input_line stdin

(* REPL *)
let repl () = 
  let exiting = ref false in 
  while (not !exiting) do
    (* prompt for a line *)
    match (prompt_line ">>> ") with
    (* parse and pretty print line *)
    | Some line -> printf "%a\n%!" (pp_parser_result pp_expr) (program line)
    (* at eof, set exit flag *)
    | None -> exiting := true
  done

