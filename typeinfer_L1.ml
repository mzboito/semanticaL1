type variable = string
(* Outros operadores binário e unários podem ser adicionados a linguagem *) 

type operator = Sum | Diff | Mult | Div | Eq | Leq 

type tipo  = TyInt | TyBool | TyFn of tipo * tipo 

type expr = Num of int 
          | Bool of bool 
          | Bop of operator * expr * expr
          | If of expr * expr * expr 
          | Var of variable 
          | App of expr * expr 
          | Lam of variable * tipo * expr 
          | Let of variable * tipo * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr

type value = Vnum of int 
           | Vbool of bool 
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and  
     env = (variable * value) list 

exception Eval_Error of string

(**********************************************************************************)

type t = 
	  TyBool 
	| TyNat 
	| TyInt ;;

exception InvalidType ;;

(* Atualizar o ambiente *) 

let update variable value environment : env = match environment with
	|[] -> [(variable, value)]
	| hd::tl -> List.append [(variable, value)] environment
		
(* procurar a variavel no ambiente *)

let rec lookup variable environment : value = match environment with
	| [] -> raise Not_found
	| (name, v)::tl ->
		if (name = variable)   (* achou a variavel no ambiente*)
			then v		(*retorna o valor dela*)
		else lookup variable tl
			
			
let rec typecheck t = match t with
	  TmInt -> TyInt (* T-Int *)
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
	| (* T-Var *)
	| (* T-Fun *)
	| (* T-App *)
	| (* T-Let *)
	| (* T-LetRec *) ;;
