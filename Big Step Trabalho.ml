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

let rec eval (amb,e) = match e with
	  Num(n) -> Vnum(n)
	| Bool(b) -> Vbool(b)
	| Bop(op,e1,e2) -> 
		(let v1 = eval (amb,e1) in
		 let v2 = eval (amb,e2)
		in (match (op,v1,v2) with
		   | (Sum, Vnum n1, Vnum n2) -> Vnum(n1 + n2)
		   | (Diff, Vnum n1, Vnum n2) -> Vnum(n1 - n2)
		   | (Mult, Vnum n1, Vnum n2) -> Vnum(n1 * n2)
		   | (Div, Vnum n1, Vnum n2) -> Vnum(n1 / n2)
		   | (Eq, Vnum n1, Vnum n2) -> Vbool(n1 == n2)
		   | (Leq, Vnum n1, Vnum n2) -> Vbool(n1 <= n2)
		   | _ -> raise (Eval_Error("TYPE ERROR IN BINARY OPERATION"))))
	| If(e1,e2,e3) -> 
		(let v1 = eval (amb,e1)
		in (match v1 with 
		   | Vbool(true) -> eval(amb,e2)
		   | Vbool(false) -> eval(amb,e3)
   		   | _ -> raise (Eval_Error("TYPE ERROR EXPRESSION IF"))))
    | Var(x) -> lookup(amb,x) (* Fazer função auxiliar lookup *)
	| App(e1,e2) ->
		let v1 = eval(amb,e1)
		    v2 = eval(amb,e2)
		in (match v1 with 
		   | Vclos(x,e,amb') -> (let amb'' = update(amb',x,v2) in eval(amb''e)) (* Fazer função auxiliar update *) (* Dúvida qual OP*)
		   | Vrclos(f,x,e,amb') -> (let amb'' = update(update(amb',f,v1),x,v2) in eval(amb'',e)) (* Fazer função auxiliar update *) (* Dúvida qual OP*)
		   | _ -> raise (Eval_Error("TYPE ERROR LEFT SIDE OF APP")))
	| Lam(x,_,e) -> Vclos(x,e,amb) (* Dúvida qual OP*)
	| Let(x,_,e1,e2)  -> (* Dúvida qual OP*)
		let v1 = eval(amb,e1)
			amb' = update(amb,x,v1)
		in eval(amb',e2)
	| Lrec(f,_,_,x,_,e1,e2) -> (* Dúvida qual OP*)
		let amb' = update(amb,f,Vrclos(f,x,e1,amb))
		in eval(amb',e2)