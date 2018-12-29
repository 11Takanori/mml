open Eval

let eval env lexer showError =
  (try
      let decl = Parser.toplevel Lexer.main lexer in
      let (id, newenv, v) = eval_decl env decl in
        Printf.printf "val %s = " id;
        pp_val v;
        print_newline();
        newenv
      with Failure str -> showError str
      | Eval.Error str -> showError str
      | Parsing.Parse_error -> showError "Parse Error"
      | _ -> showError "Other Execption")

let rec eval_stdin env =
  print_string "# ";
  flush stdout;
  let showError str = Printf.printf "%s" str;
    print_newline();
    eval_stdin env in
  let newenv = eval env (Lexing.from_channel stdin) showError in
  eval_stdin newenv

let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10) Environment.empty))

let _ = eval_stdin initial_env
