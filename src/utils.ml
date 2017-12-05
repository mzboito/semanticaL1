#use "term_L1.ml" ;;

exception Variable_not_found ;;

(* Size function for lists *)
let rec size x = match x with
     [] -> 0
   | _::tail_x -> 1 + (size tail_x) ;;

(* Function to transform term in string *)
let rec type2string t = match t with
    TyInt -> "int"
    | TyBool -> "bool"
    | TyFn(t1,t2) -> let t1' = type2string(t1) in let t2' = type2string(t2) in "fun("^t1'^"->"^t2'^")" ;;

(* Function to update environment. Arguments: variable, value, env *)
let update variable value environment : env = match environment with
  | [] -> [(variable, value)]
  | hd::tl -> List.append [(variable, value)] environment ;;

(* Function to update environment. Arguments: variable, expr, env*)

let updateExpr variable expr environment : envExp = match environment with
  | [] -> [(variable, expr)]
  | hd::tl -> List.append [(variable, expr)] environment ;;
  
(* Function to look up for variable in environment. Return a value  *)
let rec lookup variable environment : value = match environment with
  | [] -> raise Variable_not_found
  | (name, v)::tl ->
    if (name == variable)    (* Found the variable in the head *)
    then v                  (* Returns variable value *)
    else lookup variable tl ;; (* Look for it in the tale *)
    
   (* Function to look up for variable in environment. Return a expr  *)
let rec lookupExpr variable environment : expr = match environment with
  | [] -> raise Variable_not_found
  | (name, v)::tl ->
    if (name == variable)    (* Found the variable in the head *)
    then v                  (* Returns variable value *)
    else lookupExpr variable tl ;; (* Look for it in the tale *)
