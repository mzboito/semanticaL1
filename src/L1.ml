(* Libraries *)
#use "utils.ml" ;;
#use "term_L1.ml" ;;
(*#use "bigstep_L1.ml" ;; tá de castigo por dar erro *)
#use "typeinfer_L1.ml" ;;


(*  TESTAR PROGRAMAS AQUI *)

(* TESTES PARA ENV *)

(*let umNum =  Vnum (7);;
let umEnv : env = [] ;;

(* variaveis sao strings *)

let up = update "umNum" umNum umEnv;;
let search = lookup "umNum" umEnv;;*)


	(*  TmInt -> TyInt (* T-Int *)
	| TmBool -> TyBool (* T-Bool *)

	| TmOpSum(t1,t2) -> (* Pode isso????? *)
		if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) + typecheck(t2)) == TyInt else raise InvalidType (* T-Op+ *)
	| TmOpDiff(t1,t2) ->
		if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) - typecheck(t2)) == TyInt else raise InvalidType (* T-Op- *)
	| TmOpMult(t1,t2) ->
	 	if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) * typecheck(t2)) == TyInt else raise InvalidType (* T-Op* *)
	| TmOpDiv(t1,t2) ->
	 	if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) / typecheck(t2)) == TyInt else raise InvalidType (* T-Op/ *)
	| TmOpEqual(t1,t2) ->
		if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) == typecheck(t2)) == TyBool else raise InvalidType (* T-Op== *)
	| TmOpLessEqual(t1,t2) ->
		if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) <= typecheck(t2)) == TyBool else raise InvalidType (* T-Op<= *)
	| TmOpGreaterEqual(t1,t2) ->
		if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) >= typecheck(t2)) == TyBool else raise InvalidType (* T-Op>= *)
	| TmOpLess(t1,t2) ->
	    if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) < typecheck(t2)) == TyBool else raise InvalidType (* TOp< *)
	| TmOpGreater(t1,t2) ->
		if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) > typecheck(t2)) == TyBool else raise InvalidType (* TOp> *)
	| TmOpNeg(t1,t2) ->
	 	if(typecheck(t1) == TyInt && typecheck(t2) == TyInt) then (typecheck(t1) != typecheck(t2)) == TyBool else raise InvalidType (* TOp!= *)
	| TmIf(t1,t2,t3) ->
		if(typecheck(t1)) == TyBool && (typecheck(t2)) == (typecheck(t3)) then typecheck(t3) else raise InvalidType (* T-If *)
	| TmVar(variable) -> lookup variable environment *)

  (* isso retorna um valor, é só isso?? *)
	(*|  T-Fun *)
	(*|  T-App *)
	(*|  T-Let *)
	(*|  T-LetRec *)



(*let t1 = ;;
let t2 = ;;
let t3 = ;;
let t4 = ;;
let t5 = ;;

let evalList = t1:: (t2:: (t3:: (t4:: (t5 :: [])))) ;;
let output = List.map (fun x -> typecheck(x)) evalList ;;*)

(* tirei da minha implementação de L0, tem que adaptar
let printOutput elem =
    match elem with
      TmZero -> Printf.printf "Result: Value (TmZero)\n"
      | TmTrue -> Printf.printf "Result: Value (TmTrue)\n"
      | TmFalse -> Printf.printf "Result: Value (TmFalse)\n"
      | _ -> Printf.printf "Result: Error\n"
    in List.iter printOutput output ;;
*)
