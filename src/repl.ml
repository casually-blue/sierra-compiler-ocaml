let prompt_line prompt = 
  let open Core.In_channel in
  let open Core.Out_channel in 

  output_string stderr prompt;
  flush stderr;
  input_line stdin
    ~fix_win_eol: true

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
      printf "%a\n" (pp_parser_result pp_expr) (program line);
      flush stdout
    | None -> exiting := true
  done

