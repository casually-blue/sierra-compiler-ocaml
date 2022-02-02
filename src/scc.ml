let main input _ () = 
  match input with
    | "-" -> Repl.repl ()
    | input -> print_endline ("Input file: " ^ input)

let () = 
  Core.Command.run (Core.Command.basic
    ~summary: "Compile sierra source code"
    ~readme:(fun () -> "Sierra compiler Version 1.0.0")
    Core.Command.Let_syntax.(
      let%map_open
        output = flag "-o" (optional string)
          ~doc: "Output file"
        and input = anon (maybe_with_default "-" ("INPUT" %: Core.Filename.arg_type))
      in (main input output)))

