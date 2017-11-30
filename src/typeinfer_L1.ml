#use "term_L1.ml" ;;
#use "utils.ml" ;;

exception InvalidType ;;

let rec typecheck environment exp : tipo = match exp with
	(* If expression is a value *)
	  Num(exp)  -> TyInt
	| Bool(exp) -> TyBool

	(* Binary operations *)
	| Bop(op,exp1,exp2) ->
		let ope1 = typecheck environment exp1 in
		let ope2 = typecheck environment exp2 in
		(match op, ope1, ope2 with
			    Sum, TyInt, TyInt -> TyInt
				| Diff, TyInt, TyInt -> TyInt
				| Mult, TyInt, TyInt -> TyInt
				| Div, TyInt, TyInt -> TyInt
				| Equal, TyInt, TyInt -> TyBool
				| Equal, TyBool, TyBool -> TyBool
				| NotEqual, TyBool, TyBool -> TyBool
				| NotEqual, TyInt, TyInt -> TyBool
				| GreaterOrEqual, TyInt, TyInt -> TyBool
				| And, TyBool, TyBool -> TyBool
				| Or, TyBool, TyBool -> TyBool
				| Less, TyInt, TyInt -> TyBool
				| LessOrEqual, TyInt, TyInt -> TyBool
				| Greater, TyInt, TyInt -> TyBool
				| _ -> raise InvalidType
			)
		(* IF e1 then e2 else e3
			e1: bool	e2: T	e3:T	if then else: T*)
		| If(e1,e2,e3) ->
			let tipoe1 = typecheck environment e1 in
			let tipoe2 = typecheck environment e2 in
			let tipoe3 = typecheck environment e3 in
				if tipoe1 == TyBool && tipoe2 == tipoe3 then tipoe2 else raise InvalidType

		| Fun(variable, t, exp) ->
			let tipoExp =  typecheck environment exp in
			let tipoVar = typecheck environment (lookup variable environment) in
			if tipoVar == t then tipoExp else raise InvalidType(*TyFn(t,  tipoExp) else raise InvalidType
(* From this point, things need to be tested *)

(*
| Var(variable) -> typecheck environment (lookup variable environment) *)
(* lookup retorna um valor, ver o tipo desse valor*)

	(*ADICIONAR O FUN AQUI *)

(*  se num env, e1 é t -> t'   e e2 é t  , e1 e2 é t'  TyFn of tipo * tipo  *)
	| App(exp1,exp2) ->
		let appexp1 = typecheck environment exp1 in (* tem que ser t->t' *)
		let appexp2 = typecheck environment exp2 in (* tem que ser t *)
		(match appexp1 with
			TyFn(tipo1,tipo2) -> if tipo1 == appexp2 then tipo2 else raise InvalidType
			| _ -> raise InvalidType)

	(*ADICIONAR LET AQUI*)
  | Let(variable,t,exp1,exp2) ->
		let tipovar = typecheck environment (lookup variable environment) in
		let tipoexp1 = typecheck environment exp1 in
		let tipoexp2 = typecheck environment exp2 in
		if (tipovar == t && tipovar = tipoexp1) then tipoexp2 else raise InvalidType
*)
	| _ -> raise InvalidType ;; (*so pra ir debugando sem ele reclamar de falta de match*)




	(*ADICIONAR LET REC AQUI*)

	(* OLD VERSION
	Fn: variavel, tipo, expressao
		num env onde a variavel e do tipo t, a expressao e do tipo t', entao
		a funcao e do tipo t -> t'

		Fun of variable * tipo * expr
		*)

	(*	| Fun(variable, t, exp) ->
		let tipoExp =  typecheck environment exp in
		(match tipoExp with
			TyFn(t,  (typecheck environment (lookup variable environment)) )
			| _ -> raise InvalidType

		)*)


	(* Let of variable * tipo * expr * expr  *)

	(*	| Let (variable, t, exp1,exp2) ->
		let tipoExp1 = typecheck environment exp1 in
		let tipoExp2 = typecheck environment exp2 in

		;; *)
