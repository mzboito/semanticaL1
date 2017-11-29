#use "term_L1.ml" ;;


exception InvalidType ;;

(* Atualizar o ambiente *)
let update variable value environment : env = match environment with
	|[] -> [(variable, value)]
	| hd::tl -> List.append [(variable, value)] environment

(* procurar a variavel no ambiente *)
let rec lookup variable environment = match environment with
	| [] -> raise Not_found
	| (name, v)::tl ->
		if (name == variable)   (* achou a variavel no ambiente*)
			then v		(*retorna o valor dela*)
		else lookup variable tl


(* recebe um anbiente, uma expressao e retorna um tipo*)
let rec typecheck environment exp : tipo = match exp with
	(* se é exp é um valor *)
	Num(exp) -> TyInt
	| Bool(exp) -> TyBool
  (* se exp é uma operacao *)
	| Bop(op,exp1,exp2) ->
  	let ope1 = typecheck environment exp1 in (* Verificar exp1 *)
		let ope2 = typecheck environment exp2 in (* Verificar exp2 *)
		(match op, ope1, ope2 with
			| Sum, TyInt, TyInt -> TyInt
			| Diff, TyInt, TyInt -> TyInt
			| Mult, TyInt, TyInt -> TyInt
			| Div, TyInt, TyInt -> TyInt
			| Eq, TyInt, TyInt -> TyBool
			| Leq, TyInt, TyInt -> TyBool
			| _ -> raise InvalidType)
(* IF e1 then e2 else e3
e1: bool	e2: T	e3:T	if then else: T*)
	| If(e1,e2,e3) ->
		let tipoe1 = typecheck environment e1 in
		let tipoe2 = typecheck environment e2 in
		let tipoe3 = typecheck environment e3 in
		if tipoe1 == TyBool && tipoe2 == tipoe3 then tipoe2 else raise InvalidType

	| Var(variable) -> typecheck environment (lookup variable environment)
	(* lookup retorna um valor, ver o tipo desse valor*)

	(*ADICIONAR O FUN AQUI *)

(*  se num env, e1 é t -> t'   e e2 é t  , e1 e2 é t'  TyFn of tipo * tipo  *)
	| App(exp1,exp2) ->
		let appexp1 = typecheck environment exp1 in (* tem que ser t->t' *)
		let appexp2 = typecheck environment exp2 in (* tem que ser t *)
		(match appexp1 with
			TyFn(tipo1,tipo2) -> if tipo1 == appexp2 then tipo2 else raise InvalidType
			| _ -> raise InvalidType)


	| _ -> raise InvalidType ;; (*so pra ir debugando sem ele reclamar de falta de match*)

	(*
	Fn: variavel, tipo, expressao
		num env onde a variavel é do tipo t, a expressao é do tipo t', então
		a funcao é do tipo t -> t'

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
