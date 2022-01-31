let prompt_line prompt = 
  let open Core.In_channel in
  let open Format in
  let open Fmt in

  fprintf stderr "%s%!" prompt;
  input_line stdin

(* REPL *)
let repl () = 
  let exiting = ref false in 

  let open Format in
  let open Parserlib.Types in
  let open Ast in
  let open Parser in

  while (not !exiting) do
    (* prompt for a line *)
    match (prompt_line ">>> ") with
    (* parse and pretty print line *)
    | Some line -> 
      printf "%a\n%!" (pp_parser_result pp_expr) (program line)
    | None -> exiting := true
  done

