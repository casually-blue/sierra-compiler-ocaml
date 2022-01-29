open Core
open In_channel
open Out_channel

open Parserlib.Errors

open Ast
open Parser

(* REPL *)
let () = let exiting = ref false in while (not !exiting) do
  (* prompt for a line *)
  print_string ">> ";
  flush stdout;
  let line = input_line stdin
    ~fix_win_eol: true in

  (* parse the line *)
  match line with
  | Some line -> (
    (* Parse the line *)
    match (program line) with
    | Ok (e, _) -> print_string (expression_to_string e)                 
    | Error (err, input) -> print_endline ("Error parse failed:" ^ (stringify_parser_error err) ^ " (\"" ^ input ^ "\")");)
  | None -> exiting := true
done
