let prompt_line prompt = 
  let open Core.In_channel in
  let open Core.Out_channel in 

  output_string stderr prompt;
  flush stderr;
  input_line stdin
    ~fix_win_eol: true

let print_pres pr = 
  Format.printf "%a\n" (Parserlib.Types.pp_parser_result Ast.pp_expr) pr;
  flush stdout

(* REPL *)
let repl () = let exiting = ref false in while (not !exiting) do
  (* prompt for a line *)
  match (prompt_line ">>> ") with
  | Some line -> print_pres (Parser.program line)
  | None -> exiting := true
done

