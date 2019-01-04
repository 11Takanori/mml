open Eval
open Typing

let eval env tyenv lexer showError =
  try
    let decl = Parser.toplevel Lexer.main lexer in
    (* let ty = ty_decl tyenv decl in *)
    let id, newenv, v = eval_decl env decl in
    Printf.printf "val %s : " id ;
    (* Syntax.pp_ty ty ; *)
    print_string " = " ;
    pp_val v ;
    print_newline () ;
    newenv
  with
  | Failure str -> showError str
  | Eval.Error str -> showError str
  | Typing.Error str -> showError str
  | Parsing.Parse_error -> showError "Parse Error"
  | _ -> showError "Other Exception"

let rec eval_stdin env tyenv =
  print_string "# " ;
  flush stdout ;
  let showError str =
    Printf.printf "%s" str ; print_newline () ; eval_stdin env tyenv
  in
  let newenv = eval env tyenv (Lexing.from_channel stdin) showError in
  eval_stdin newenv tyenv

let eval_file filename env tyenv =
  let showError str = Printf.printf "%s" str ; print_newline () ; env in
  eval env tyenv (Lexing.from_channel (open_in filename)) showError

let initial_env = Environment.empty

let initial_tyenv = Environment.empty

let _ =
  if Array.length Sys.argv > 1 then
    eval_file Sys.argv.(1) initial_env initial_tyenv
  else eval_stdin initial_env initial_tyenv
