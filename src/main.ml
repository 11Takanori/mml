open Eval

let eval env lexer show_error =
  (try
      let decl = Parser.toplevel Lexer.main lexer in
      let (id, newenv, v) = eval_decl env decl in
        Printf.printf "val %s = " id;
        pp_val v;
        print_newline();
        newenv
      with Failure str -> show_error str
      | Eval.Error str -> show_error str
      | Parsing.Parse_error -> show_error "Parse Error"
      | _ -> show_error "Other Execption")

let rec eval_stdin env =
  print_string "# ";
  flush stdout;
  let show_error str = Printf.printf "%s" str;
    print_newline();
    eval_stdin env in
  let newenv = eval env (Lexing.from_channel stdin) show_error in
  eval_stdin newenv

let eval_file filename env =
  let show_error str = Printf.printf "%s" str;
    print_newline();
    env in
  eval env (Lexing.from_channel (open_in filename)) show_error

let initial_env = Environment.empty

let _ =
  if (Array.length Sys.argv) > 1 then eval_file Sys.argv.(1) initial_env
  else eval_stdin initial_env