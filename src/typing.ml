open Syntax

exception Error of string

let err s = raise (Error s)

type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let rec eqs_of_subst s =
  match s with
  | [] -> []
  | (tyvar, ty) :: rest -> (TyVar tyvar, ty) :: eqs_of_subst rest

let rec subst_type s typ =
  let rec resolve_type s = function
    | TyVar v -> ( try List.assoc v s with Not_found -> TyVar v )
    | TyFun (ty1, ty2) -> TyFun (resolve_type s ty1, resolve_type s ty2)
    | a -> a
  in
  let rec resolve_subst = function
    | [] -> []
    | (id, typ) :: rest ->
        let new_subst = resolve_subst rest in
        (id, resolve_type new_subst typ) :: new_subst
  in
  resolve_type (resolve_subst s) typ

let rec subst_eqs s eqs =
  match eqs with
  | [] -> []
  | (ty1, ty2) :: rest ->
      (subst_type s ty1, subst_type s ty2) :: subst_eqs s rest

let rec unify = function
  | [] -> []
  | (ty1, ty2) :: rest -> (
    match (ty1, ty2) with
    | TyInt, TyInt -> unify rest
    | TyBool, TyBool -> unify rest
    | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
        unify ((ty12, ty22) :: (ty11, ty21) :: rest)
    | TyVar var1, TyVar var2 ->
        if var1 = var2 then unify rest
        else
          let eqs = [(var1, ty2)] in
          eqs @ unify (subst_eqs eqs rest)
    | TyVar var, ty | ty, TyVar var ->
        if MySet.member var (Syntax.freevar_ty ty) then err "Type err"
        else
          let eqs = [(var, ty)] in
          eqs @ unify (subst_eqs eqs rest)
    | _, _ -> unify rest )

let ty_prim op ty1 ty2 =
  match op with
  | Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Minus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | Or -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)

let rec ty_exp tyenv = function
  | Var x -> (
    try ([], Environment.lookup x tyenv) with Environment.Not_bound ->
      err ("Variable not bound: " ^ x) )
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      let eqs3, ty = ty_prim op ty1 ty2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 @ eqs3 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let scond, tycond = ty_exp tyenv exp1 in
      let sthen, tythen = ty_exp tyenv exp2 in
      let selse, tyelse = ty_exp tyenv exp3 in
      let eqs3 = [(tycond, TyBool); (tythen, tyelse)] in
      let eqs =
        eqs_of_subst scond @ eqs_of_subst sthen @ eqs_of_subst selse @ eqs3
      in
      let s3 = unify eqs in
      (s3, subst_type s3 tythen)
  | LetExp (id, exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs3 = [(domty, ty1)] in
      let eqs = eqs_of_subst s1 @ eqs3 @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty = ty_exp (Environment.extend id domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      let ty = TyVar (fresh_tyvar ()) in
      let eqs =
        ((ty1, TyFun (ty2, ty)) :: eqs_of_subst s1) @ eqs_of_subst s2
      in
      let s3 = unify eqs in
      (s3, subst_type s3 ty)
  | LetRecExp (id1, id2, exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp (Environment.extend id1 ty1 tyenv) exp2 in
      let s3, ty3 = ty_exp (Environment.extend id2 ty2 tyenv) exp1 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs3 = [(domty, ty1); (domty, ty3)] in
      let eqs = eqs_of_subst s1 @ eqs3 @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)

let ty_decl tyenv = function
  | Exp e -> ty_exp tyenv e
  | Decl (_, e) -> ty_exp tyenv e
  | _ -> err "Not Implemented"
