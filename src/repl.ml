open Parserlib.Types

open Ast
open Parser

let prompt_line prompt = 
  Format.fprintf Fmt.stderr "%s%!" prompt;
  Core.In_channel.input_line Core.In_channel.stdin

(* REPL *)
let repl () = 
  let exiting = ref false in 
  while (not !exiting) do
    (* prompt for a line *)
    match (prompt_line ">>> ") with
    (* parse and pretty print line *)
    | Some line -> Format.printf "%a\n%!" (pp_parser_result (Format.pp_print_option pp_expr)) (program line)
    (* at eof, set exit flag *)
    | None -> exiting := true
  done

