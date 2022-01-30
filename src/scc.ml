open Core

open Parserlib.Errors

open Ast
open Parser

let prompt_line prompt = 
  let open In_channel in
  let open Out_channel in 

  output_string stderr prompt;
  flush stderr;
  input_line stdin
    ~fix_win_eol: true

let print_string_flush s = 
  let open Out_channel in
  print_string s; 
  flush stdout

(* REPL *)
let repl = (fun () -> let exiting = ref false in while (not !exiting) do
  (* prompt for a line *)
  (* parse the line *)
  match (prompt_line ">>> ") with
  | Some line -> (
    (* Parse the line *)
    match (program line) with
    | Ok (e, _) -> 
        print_string_flush (expression_to_string e)                 
    | Error (err, input) -> 
        print_endline ("Error parse failed:" ^ (stringify_parser_error err) ^ " (\"" ^ input ^ "\")");)
  | None -> exiting := true

done)

let scc = 
  Command.basic
    ~summary: "Compile sierra source code"
    ~readme:(fun () -> "Sierra compiler Version 1.0.0")
    Command.Let_syntax.(
      let%map_open
        _ = flag "-o" (optional string)
          ~doc: "Output file"
        and input = anon (maybe_with_default "-" ("INPUT" %: Filename.arg_type))
      in fun () ->
        match input with
          | "-" -> repl (); ()
          | input -> print_endline ("Input file: " ^ input))

let () = Command.run scc

