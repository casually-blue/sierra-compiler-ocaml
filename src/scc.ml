open Batteries

open Parserlib.Errors

open Ast
open Parser

module P = BatOptParse

let input_opt = P.StdOpt.str_option ~metavar:"STRING" ()

let optparser : P.OptParser.t = 
  P.OptParser.make
    ~prog: "scc"
    ~usage: "%prog - compile sierra source code"
    ~version: "0.1"
    ()

let sl  = P.OptParser.parse_argv optparser

let rec print_string_list sl = match sl with
  | [] -> ""
  | s :: l -> s ^ " " ^ (print_string_list l)

let () = print_string (print_string_list sl)

(* REPL *)
let () = let exiting = ref false in while (not !exiting) do
  (* prompt for a line *)
  print_string ">> ";
  let line = (try Some(read_line()) with End_of_file -> None) in

  (* parse the line *)
  match line with
  | Some line -> (
    (* Parse the line *)
    match (program line) with
    | Ok (e, _) -> print_string (expression_to_string e)                 
    | Error (err, input) -> print_endline ("Error parse failed:" ^ (stringify_parser_error err) ^ " (\"" ^ input ^ "\")");)
  | None -> exiting := true
done
