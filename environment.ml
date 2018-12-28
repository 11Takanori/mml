type 'a t = (Syntax.id * 'a) list

exception Not_bound

let empty = []
let extend x v env = (x,v)::env

let lookup x env = 
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::t -> (id, f v) :: map f t

let rec fold_right f env a = 
  match env with
      [] -> a
    | (_, v)::t -> f v (fold_right f t a)
