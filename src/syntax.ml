type id = string

type binOp = Plus | Minus | Mult | Lt | And | Or

type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | LetRecExp of id * id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp

type program = Exp of exp | Decl of id * exp | RecDecl of id * id * exp

type tyvar = int

type ty = TyInt | TyBool | TyVar of tyvar | TyFun of ty * ty

let var_id =
  let var_list = ref [] in
  let body tyvar =
    let rec index_of counter = function
      | [] ->
          var_list := !var_list @ [tyvar] ;
          counter
      | x :: rest ->
          if x == tyvar then counter else index_of (counter + 1) rest
    in
    index_of 0 !var_list
  in
  body

let rec pp_ty = function
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar tyvar ->
      Printf.printf "'%c" (char_of_int (int_of_char 'a' + var_id tyvar))
  | TyFun (ty1, ty2) -> pp_ty ty1 ; print_string " -> " ; pp_ty ty2

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1 ;
    v
  in
  body

let rec freevar_ty ty =
  match ty with
  | TyVar var -> MySet.singleton var
  | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
  | _ -> MySet.empty
