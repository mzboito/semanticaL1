type variable = string
(* Outros operadores binário e unários podem ser adicionados a linguagem *) 

type operator = Sum | Diff | Mult | Div | Eq | Leq 

type tipo  = TyInt | TyBool | TyFn of tipo * tipo 

type expr = Num of int 
          | Bool of bool 
          | Bop of operator * expr * expr
          | If of expr * expr * expr 
	  | Not of expr
          | Var of variable 
          | App of expr * expr 
	  | Fun of variable * tipo * expr
          (* | Lam of variable * tipo * expr  *)  
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
	| TyInt 
	| TyFn of tipo * tipo;; (* acho que tem que estar aqui *)

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
			
			
let rec typecheck environment exp : tipo = match exp with  (* recebe um anbiente, uma expressao e retorna um tipo*)
	
	(* se é exp é um valor *) 
	Num(exp) -> TyInt
	| Bool(exp) -> TyBool
	  
	  (* se exp é uma operacao *) 
	|  Bop(op,exp1,exp2) ->
	  	let ope1 = typecheck environment exp1  (* Verificar exp1 *)
		let ope2 = typecheck environment exp2  (* Verificar exp2 *)
		(match op, ope1, ope2 with 
		
			| Sum, TyInt, TyInt -> TyInt      
			| Diff, TyInt, TyInt -> TyInt
			| Mult, TyInt, TyInt -> TyInt
			| Div, TyInt, TyInt -> TyInt
			| Eq, TyInt, TyInt -> TyBool
			| Leq, TyInt, TyInt -> TyBool
			| _ -> raise InvalidType
		)
			
	| Not(exp) ->   (* exp deve ser do tipo bool *) 
		let note1 = typecheck environment exp
		(match note1 with
			TyBool -> TyBool
			| _ -> raise InvalidType
	  	)
		
	| Var(variable) -> typecheck environment (lookup variable environment) (* lookup retorna um valor, ver o tipo desse valor*)
	
	
(*  se num env, e1 é t -> t'   e e2 é t  , e1 e2 é t'  TyFn of tipo * tipo  *)
	| App(exp1,exp2) ->
		let appexp1 = typecheck environment exp1
		let appexp2 = typecheck environment exp2
		(match appe1 with
			TyFn(tipo1,tipo2) -> if tipo2 = appexp1 then tipo2 else raise InvalidType
			| _ -> raise InvalidType
			
		)
		
		
	(* Fn: variavel, tipo, expressao
		num env onde a variavel é do tipo t, a expressao é do tipo t', então
		a funcao é do tipo t -> t'  
		
		Fun of variable * tipo * expr
		*)
		
	| Fun(variable, t, exp) ->
		let tipoExp =  typecheck environment exp in
		(match tipoExp with
			TyFn(t,  (typecheck environment (lookup variable environment)) )
			| _ -> raise InvalidType
		
		)
		
		
	(* Let of variable * tipo * expr * expr  *)
	
	| Let (variable, t, exp1,exp2) -> 
		let tipoExp1 = typecheck environment exp1 in
		let tipoExp2 = typecheck environment exp2 in
		
		;;
		
		
		(* ..... *) 
		
(* TESTES PARA ENV *)
		
let umNum =  Vnum (7);; 
let umEnv : env = []

(* variaveis sao strings *)

let up = update "umNum" umNum umEnv;;
let search = lookup "umNum" umEnv;;
		
	
	(**  TmInt -> TyInt (* T-Int *)
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
	| TmVar(variable) -> lookup variable environment (* isso retorna um valor, é só isso??)
	| (* T-Fun *)
	| (* T-App *)
	| (* T-Let *)
	| (* T-LetRec *) ;; *)

