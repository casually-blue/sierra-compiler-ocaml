open Parserlib.Errors

open Ast
open Parser

(* REPL *)
let () = let exiting = ref false in while (not !exiting) do
        (* prompt for a line *)
        print_string ">> ";
        let line = (try Some(read_line()) with 
                End_of_file -> None
        ) in

        (* parse the line *)
        match line with
        | Some line -> (
                (* Parse the line *)
                match (programp line) with
                | Ok (f, _) -> print_string (func_to_string f)                 
                | Error (err, input) -> print_endline ("Error parse failed:" ^ (stringify_parser_error err) ^ " (" ^ input ^ ")");)
        | None -> exiting := true
done