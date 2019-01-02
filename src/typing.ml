open Syntax

exception Error of string

let err s = raise (Error s)

type tyenv = ty Environment.t

let ty_prim op ty1 ty2 =
  match op with
  | Plus -> (
    match (ty1, ty2) with
    | TyInt, TyInt -> TyInt
    | _ -> err "Argument must be integer: +" )
  | Mult -> (
    match (ty1, ty2) with
    | TyInt, TyInt -> TyInt
    | _ -> err "Argument must be integer: *" )
  | Lt -> (
    match (ty1, ty2) with
    | TyInt, TyInt -> TyBool
    | _ -> err "Argument must be integer: <" )
  | And -> (
    match (ty1, ty2) with
    | TyBool, TyBool -> TyBool
    | _ -> err "Argument must be bool: &&" )
  | Or -> (
    match (ty1, ty2) with
    | TyBool, TyBool -> TyBool
    | _ -> err "Argument must be bool: ||" )

let rec ty_exp tyenv = function
  | Var x -> (
    try Environment.lookup x tyenv with Environment.Not_bound ->
      err ("variable not bound: " ^ x) )
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) -> (
      let tycond = ty_exp tyenv exp1 in
      let tythen = ty_exp tyenv exp2 in
      let tyelse = ty_exp tyenv exp3 in
      match tycond with
      | TyBool ->
          if tythen = tyelse then tythen
          else
            err
              "Type of then expression and that of else expression must be \
               same type"
      | _ -> err "Conditon must be bool" )
  | LetExp (id, exp1, exp2) ->
      let tyvalue = ty_exp tyenv exp1 in
      ty_exp (Environment.extend id tyvalue tyenv) exp2
  | _ -> err "Not Implemented"

let ty_decl tyenv = function
  | Exp e -> ty_exp tyenv e
  | Decl (_, e) -> ty_exp tyenv e
  | _ -> err "Not Implemented"

type subst = (tyenv * ty) list

let rec subst_type s typ =
  let rec resolve_type s = function
      TyVar v -> (try List.assoc v s with Not_found -> TyVar v)
    | TyFun (ty1, ty2) -> TyFun (resolve_type s ty1, resolve_type s ty2)
    | a -> a in
  let rec resolve_subst = function
      [] -> []
    | (id, typ) :: rest -> let new_subst = resolve_subst rest in
        ((id, resolve_type new_subst typ) :: new_subst) in
  resolve_type (resolve_subst s) typ
