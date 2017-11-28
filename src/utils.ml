#use "term_L1.ml" ;;

(* Size function for lists *)
let rec size x = match x with
     [] -> 0
   | _::tail_x -> 1 + (size tail_x) ;;

(* Function to transform term in string *)
let rec type2string t = match t with
    TyInt -> "int"
    | TyBool -> "bool"
    | TyFn(t1,t2) -> let t1' = type2string(t1) in let t2' = type2string(t2) in "TyFn("^t1'^"->"^t2'^")" ;;



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
