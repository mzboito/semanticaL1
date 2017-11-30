#use "term_L1.ml" ;;
#use "typeinfer_L1.ml" ;;
#use "utils.ml" ;;


let t1 = Num(3) ;;
let t2 = Bool(false) ;;
let t3 = Bop(Sum, t1, t1) ;;

let evalList = t1:: (t2:: (t3 :: [])) ;;
let output = List.map (fun x -> (typecheck [] x)) evalList ;;
let strings = List.map (fun t -> (type2string t)) output ;;

Printf.printf "%s" (type2string (typecheck [] t1)) ;;
