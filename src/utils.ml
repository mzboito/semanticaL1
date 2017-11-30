#use "term_L1.ml" ;;

(* Size function for lists *)
let rec size x = match x with
     [] -> 0
   | _::tail_x -> 1 + (size tail_x) ;;

(* Function to transform term in string *)
let rec type2string t = match t with
    TyInt -> "int"
    | TyBool -> "bool"
    | TyFn(t1,t2) -> let t1' = type2string(t1) in let t2' = type2string(t2) in "fun("^t1'^"->"^t2'^")" ;;

(* Function to update environment *)
let update variable value environment : env = match environment with
  | [] -> [(variable, value)]
  | hd::tl -> List.append [(variable, value)] environment

(* Function to look up for variable in environment *)
let rec lookup variable environment : value = match environment with
  | [] -> raise Not_found
  | (name, v)::tl ->
    if (name = variable)    (* Found the variable in the head *)
    then v                  (* Returns variable value *)
    else lookup variable tl (* Look for it in the tale *)

  (*
  let rec generateString elem =
      match elem with
        TmZero -> "0"
        | TmTrue -> "True"
        | TmFalse -> "False"
        | TmIsZero (t1) -> let t1' = generateString(t1) in "IsZero (" ^ t1' ^ ")"
        | TmSucc (t1) -> let t1' = generateString(t1) in "TmSucc (" ^ t1' ^ ")"
        | TmPred (t1) -> let t1' = generateString(t1) in "TmPred (" ^ t1' ^ ")"
        | TmIf (t1,t2,t3) -> let t1' = generateString(t1) in
                              let t2' = generateString(t2) in
                              let t3' = generateString(t3)
                              in "TmIf (" ^ t1' ^","^ t2' ^","^ t3' ^")" ;;*)
