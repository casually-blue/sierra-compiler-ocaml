let prompt_line prompt = 
  let open Core.In_channel in
  let open Core.Out_channel in 

  output_string stderr prompt;
  flush stderr;
  input_line stdin
    ~fix_win_eol: true

let print_string_flush s = 
  let open Core.Out_channel in
  print_string s; 
  flush stdout

(* REPL *)
let repl () = let exiting = ref false in while (not !exiting) do
  (* prompt for a line *)
  (* parse the line *)
  match (prompt_line ">>> ") with
  | Some line -> (
    (* Parse the line *)
    match (Parser.program line) with
    | Ok (e, _) -> 
        print_string_flush (Ast.expression_to_string e)                 
    | Error (err, input) -> 
        print_endline ("Error parse failed:" ^ (Parserlib.Errors.stringify_parser_error err) ^ " (\"" ^ input ^ "\")"))
  | None -> exiting := true
done

