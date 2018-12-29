open Syntax 

type exval = 
    IntV of int
  | BoolV of bool
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let pp_val = function
    IntV i -> 
      print_int i
  | BoolV b -> 
      if b then print_string "true" else print_string "false"

let apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV b1, BoolV b2 -> BoolV(b1 && b2)
  | And, _, _ -> err ("Both arguments must be bool: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV(b1 || b2)
  | Or, _, _ -> err ("Both arguments must be bool: ||")

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
    let value = eval_exp env exp1 in
      eval_exp (Environment.extend id value env) exp2

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, exp) ->
      let v = eval_exp env exp in (id, Environment.extend id v env, v)