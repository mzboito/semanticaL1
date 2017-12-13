(*
	Federal University of Rio Grande do Sul
	Institute of Informatics
	INF005516 - Formal Semantics N
	Final project:
		Consists of the implementation of an interpreter composed of an expression evaluator, implemented following the
		rules of operational semantics given in the big step style, and an inference of types.
		Both must follow strictly the operational semantics and the type system defined for L1. The expression evaluator
		should be implemented following the rules of operational semantics given in the big step style.
		Extra implementations: An interpreter for the abstract machine language SSM2 and the compilation of L1 for the
		abstract machine language SSM2.
	Professor: Alvaro Freitas Moreira
	Group members: Lisiane Aguiar
             	   Marcely Zanon Boito
             	   Victoria Elizabetha Alves
*)

(* ** Terms ** *)
type variable = string

type operator = Sum | Diff | Mult | Div | Equal | NotEqual | GreaterOrEqual | And | Or | Less | LessOrEqual | Greater

type tipo  = TyInt | TyBool | TyFn of tipo * tipo

type expr = Num of int
          | Bool of bool
          | Bop of operator * expr * expr
          | If of expr * expr * expr
          | Var of variable
          | App of expr * expr
          | Fun of variable * tipo * expr
          | Let of variable * tipo * expr * expr
          | Lrec of variable * (tipo * tipo) * (variable * tipo * expr) * expr

type value = Vnum of int
           | Vbool of bool
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and
     env = (variable * value) list
and
     envt = (variable * tipo) list
and
     envExp = (variable * expr) list

exception Eval_Error of string
;;


(* ** Utils ** *)

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

(* Function to transform value in string *)
let rec value2string v : unit = match v with
	| Vnum(n) -> Printf.printf "Vnum(%d)" n
	| Vbool(b) ->Printf.printf "Vbool(%b)" b
	| Vclos (var,exp,env) -> Printf.printf "Vclos (var,exp,env)"
	| Vrclos (var,ver,exp,env) -> Printf.printf "Vrclos (var, ver, exp, env)" ;;


(* Function to update environment *)
let update variable v environment : env = match environment with
  | [] -> [(variable, v)]
  | hd::tl -> List.append [(variable, v)] environment ;;


(* Function to look up for variable in environment *)
let rec lookup variable environment : value = match environment with
  | [] -> raise Variable_not_found
  | (name, v)::tl ->
    if (name == variable)      (* Found the variable in the head *)
    then v                     (* Returns variable value *)
    else lookup variable tl ;; (* Look for it in the tale *)



(* Function to update environment of TYPES *)
let updateT variable tipo environment : envt = match environment with
  | [] -> [(variable, tipo)]
  | hd::tl -> List.append [(variable,tipo)] environment ;;



(* Function to look up for variable in environment of TYPES *)
let rec lookupT variable environment : tipo = match environment with
  | [] -> raise Variable_not_found
  | (name, v)::tl ->
    if (name == variable)        (* Found the variable in the head *)
    then v                      (* Returns variable value *)
    else lookupT variable tl ;; (* Look for it in the tale *)

(* ** Type Infer ** *)
exception InvalidType ;;

let rec typecheck environment exp : tipo = match exp with
  (* Values *)
    Num(v)  -> TyInt
  | Bool(b) -> TyBool

  (* Binary operations *)
  | Bop(op,exp1,exp2) ->
    let ope1 = typecheck environment exp1 in
    let ope2 = typecheck environment exp2 in
    (match op, ope1, ope2 with
          Sum, TyInt, TyInt -> TyInt
        | Diff, TyInt, TyInt -> TyInt
        | Mult, TyInt, TyInt -> TyInt
        | Div, TyInt, TyInt -> TyInt
        | Equal, TyBool, TyBool -> TyBool
        | Equal, TyInt, TyInt -> TyBool
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

  (* Conditional *)
  | If(e1,e2,e3) ->
      let tipoe1 = typecheck environment e1 in
      let tipoe2 = typecheck environment e2 in
      let tipoe3 = typecheck environment e3 in
          if tipoe1 == TyBool && tipoe2 == tipoe3 then tipoe2 else raise InvalidType

   (* Function *)
  | Fun(v, t, e) -> TyFn(t, (typecheck(updateT v t environment) e))

  (* Variable *)
  | Var(v) -> lookupT v environment

 (* Application *)
  | App(e1,e2) ->
    let exp1 = typecheck environment e1 in
    let exp2 = typecheck environment e2 in
    (match exp1 with
      TyFn(t1,t2) -> (if t1 == exp2
          then t2
          else raise InvalidType)
      |_ -> raise InvalidType)

  (* Let *)
  | Let(variable, t, e1, e2) ->
    let x = typecheck environment e1 in
    (if x == t (*if the expression e1 is right (of type t)*)
        then typecheck (updateT variable t environment) e2 (*ten the output is of typecheck(e2)*)
        else raise InvalidType)

  (* Let Rec *)
  | Lrec(func, (ty1, ty2), (var, ty3, e1), e2) ->
    let ty4 = TyFn(ty1,ty2) in
    let update1 = updateT var ty3 environment in
    let update2 = updateT func ty4 update1 in
    let ty5 = typecheck update2 e1 in (
      if ty5 == ty2 && ty1 == ty3
      then typecheck (updateT func ty4 environment) e2
      else raise InvalidType
    )
;;

(* ** Big step ** *)
exception InvalidEval ;;
let rec eval environment e : value = match e with

  (* Values (BS-NUM e BS-BOOL) *)
    Num(v)  -> Vnum(v)
  | Bool(b) -> Vbool(b)

  (* Binary operations  (BS-OP)*)
  | Bop(op,exp1,exp2) ->
    let ope1 = eval environment exp1 in
    let ope2 = eval environment exp2 in
    (match op, ope1, ope2 with
          Sum, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 + exp2)
        | Diff, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 - exp2)
        | Mult, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 * exp2)
        | Div, Vnum(exp1), Vnum(exp2) -> Vnum(exp1 / exp2)
        | Equal, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 = exp2)
        | Equal, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 = exp2)
        | NotEqual, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 <> exp2)
        | NotEqual, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 <> exp2)
        | GreaterOrEqual, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 >= exp2)
        | And, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 && exp2)
        | Or, Vbool(exp1), Vbool(exp2) -> Vbool(exp1 || exp2)
        | Less, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 < exp2)
        | LessOrEqual, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 <= exp2)
        | Greater, Vnum(exp1), Vnum(exp2) -> Vbool(exp1 > exp2)
	| _ -> raise InvalidEval
      )


  (*  Conditional (BS-IF)*)
  | If(e1, e2, e3) when ((eval environment e1) = Vbool(true)) -> eval environment e2
  | If(e1, e2, e3) when ((eval environment e1) = Vbool(false)) -> eval environment e3


  (* Function *)
  | Fun(v, t, e) -> Vclos(v, e, environment)


  (* Variable (BS-ID)*)
  | Var(v) -> lookup v environment


  (* Application *)
  | App(e1, e2) ->
      let exp1 = eval environment e1 in
      let exp2 = eval environment e2 in
      (match exp1, exp2 with
        Vclos(variable, e, envA), value -> eval (update variable value envA) e
        | Vrclos(f, x, e, envA), value -> eval (update f (Vrclos(f, x, e, envA)) (update x value envA)) e
        | _ -> raise InvalidEval
      )

  (* Let (BS-LET)*)
  | Let(variable, t, e1, e2) ->
      let exp1 = eval environment e1 in (*evaluate e1*)
      let env2 = (update variable exp1 environment) in (*creates {x->v} + env*)
      eval env2 e2 (*evaluates e2 in this new environment*)

  (* Let Rec *)
  | Lrec(f, (t1, t2), (variable, t3, e1), e2) -> (*{let rec f = fn x => e1 in e2}*)
      let exp1 = eval environment (Fun(variable, t3, e1)) in
      let recenv = update f exp1 environment in (*{f -> {f,variable,e1,env}}*)
      let evale2 = eval recenv e2 in
      (* let exp1 = eval environment (Fun(variable, t3, e1)) in*)
      (match exp1 with (*there are some things missing......*)
        | Vclos(x, e, env) -> eval (update f (Vrclos(f, x, e, environment)) env) e2
        | _ -> raise InvalidEval
      )

	| _ -> raise InvalidEval
;;

(* ** Tests Type Infer ** *)

let t1 = Num(3) ;;
let t2 = Bool(true) ;;
let t3 = Bop(Sum, t1, t1) ;;
let t4 = Bop(Equal, t1, t1) ;;
let t5 = Bop(Equal, t2, t2) ;;
let t6 = Bop(Equal, t2, t1) ;; (* deve dar erro *)
let t7 = Bop(NotEqual, t1, t1) ;;
let t8 = Bop(GreaterOrEqual, t1, t1) ;;
let t9 = Bop(And, t2, t2) ;;
let t10 = Bop(Or, t2, t2) ;;
let t11 = Bop(Less, t1, t1) ;;
let t12 = Bop(LessOrEqual, t1, t1) ;;
let t13 = Bop(Greater, t1, t1) ;;
let t14 = Bop(Diff, t1, t1) ;;
let t15 = Bop(Mult, t1, t1) ;;
let t16 = Bop(Div, t1, t1) ;;
let t17 = Bop(NotEqual, t2, t1) ;; (* erro *)
let t18 = If(t2,t3,t15) ;; (*deve dar int*)
let t19 = If(t2,t4,t5) ;; (*deve dar bool*)
let t20 = If(t2,t3,t5) ;; (*deve dar erro*)
let env = [] ;;
let t21 = "variavelInt" ;;
let currentEnv = updateT (t21) (TyInt) (env);;
let t22 = Fun(t21,TyInt,t15);; (* int -> int *)
let t23 = Fun(t21,TyInt,t5);; (* int -> bool *)
let t24 = "variavelBool" ;;
let currentEnv2 = updateT (t24) (TyBool) (env) ;;
let t25 = App(t22,t1) ;; (* t22: int -> int   t1: int    e1e2: int *)
let t27 = App(t23,t14) ;; (* t23:  int -> bool   e2: int    e1e2: bool , *)
let t26 = App(t23,t7) ;; (* t23: int -> bool   e2: bool    e1e2: bool , erro *)

(* e1: T  vaar: T  e2: T', let é T' *)
let t28 = Let(t24,TyBool, t19, t16) ;;
let t29 = Let(t21,TyInt, t16, t19) ;;
let t30 = Let(t21,TyInt, t19, t16) ;; (* deve dar erro *)
let t31 = Lrec(t21,(TyInt,TyInt),(t21,TyInt,t15),t16);;
let t32 = Lrec(t24,(TyBool,TyBool),(t24,TyBool,t5),t12);;
let t33 = Lrec(t24,(TyBool,TyInt),(t24,TyBool,t5),t12);; (* deve dar erro *)

(* Testes para let rec - ok
Printf.printf "Verificando tipo let rec: %s" (type2string (typecheck (currentEnv) (t31))) ;;
print_newline();;
Printf.printf "Verificando tipo let rec: %s" (type2string (typecheck (currentEnv2) (t32))) ;;
print_newline();;
Printf.printf "Verificando tipo let rec: %s" (type2string (typecheck (currentEnv2) (t33))) ;; *)

(* Testes para let
Printf.printf "Verificando tipo let: %s" (type2string (typecheck (currentEnv2) (t28))) ;; (* currentEnv2 porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;
Printf.printf "Verificando tipo let: Let(variavelInt,TyInt, Bop(Div, Num(3), Num(3)), If(Bool(true), Bop(Equal, Num(3), Num(3)),Bop(Equal, Bool(true), Bool(true))))  %s" (type2string (typecheck (currentEnv) (t29))) ;; (* currentEnv2 porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;
Printf.printf "Verificando tipo let: Let( variavelInt,TyInt, If(Bool(true), Bop(Equal, Num(3), Num(3) ), Bop (Equal, Bool(true), Bool(true) ) ), Bop(Div, Num(3), Num(3))) %s" (type2string (typecheck (currentEnv) (t30))) ;; (* currentEnv2 porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;
*)
(* Testes para app - ok
Printf.printf "Verificando tipo app: %s" (type2string (typecheck (currentEnv) (t25))) ;; (* currentEnv porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;
Printf.printf "Verificando tipo app: App( Fun(variavelInt,TyInt, Bop(Equal,  Bool(true),  Bool(true)) ) , Bop(Diff, Num(3), Num(3))) %s" (type2string (typecheck (currentEnv) (t27))) ;; (* currentEnv porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;
Printf.printf "Verificando tipo app: App( Fun(variavelInt,TyInt, Bop(Equal,  Bool(true),  Bool(true)) ), Bop(NotEqual, Num(3), Num(3)) ) %s" (type2string (typecheck (currentEnv) (t26))) ;; (* currentEnv porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;
*)

(* Testes para tipo Num, Bool, Bop - Ok
Printf.printf "Verificando tipo num: %s" (type2string (typecheck [] t1)) ;;
print_newline();;
Printf.printf "Verificando tipo bool: %s" (type2string (typecheck [] t2)) ;;
print_newline();;
Printf.printf "Verificando tipo op Sum: %s" (type2string (typecheck [] t3)) ;;
print_newline();;
print_newline();;
Printf.printf "Verificando tipo op Equal: %s" (type2string (typecheck [] t4)) ;;
print_newline();;
Printf.printf "Verificando tipo op Equal: %s" (type2string (typecheck [] t5)) ;;
print_newline();;
Printf.printf "Verificando tipo op Equal: %s" (type2string (typecheck [] t6)) ;;
*)

(* Testes para o if - ok
print_newline();;
Printf.printf "Verificando if: %s" (type2string (typecheck [] t18)) ;;
print_newline();;
Printf.printf "Verificando if: %s" (type2string (typecheck [] t19)) ;;
print_newline();;
Printf.printf "Verificando if: %s" (type2string (typecheck [] t20)) ;;
print_newline();;
*)

(* Testes para o fun - ok
Printf.printf "Verificando fun: %s" (type2string (typecheck currentEnv t22)) ;; (* variavel t21 definida dentro do currentenv*)
print_newline();;
Printf.printf "Verificando fun: %s" (type2string (typecheck currentEnv t23)) ;;
print_newline();;
*)
(* Testes para var - ok

Printf.printf "Verificando var: %s" (type2string (typecheck (currentEnv) (Var(t21))) ) ;;
print_newline();;
Printf.printf "Verificando var: %s" (type2string (typecheck (currentEnv2) (Var(t24))) ) ;;
print_newline();;
*)

(* Testes Big Step *)


let evalList = t1:: (t2:: (t3 :: [])) ;;
let output = List.map (fun x -> (typecheck [] x)) evalList ;;
let strings = List.map (fun t -> (type2string t)) output ;;

(* CONJUNTO DE TESTES *)
let numTesting = Vnum(1);;
let boolTesting = Vbool(true);;

let varInt = "varTest";;
let valueEnv =  update varInt numTesting [];;
let varBool = "varTestBool";;
let valueEnv2 =  update varBool boolTesting [];;

let vclosTesting = Vclos(varInt,t22,valueEnv);;
let vclosTesting = Vclos(varBool,t22,valueEnv2);;
let vclosTesting = Vrclos(varBool,varBool,t4,valueEnv2);;

let expBopE = Bop(Equal,Bool(true),Bool(false));;
let expBopE2 = Bop(Equal,Bool(false),Bool(true));;
let expBopSum = eval [] (Bop(Sum,Num(1),Num(2))) ;;
let expBopEqual= eval [] (Bop(Equal,Bool(true),Bool(false))) ;;

let ifexp = If(expBopE,Bool(true),Bool(false)) ;;
let ifexpInt = If(expBopE,Bool(true),Num(3)) ;;

let funTest = Fun(varInt,TyInt,Bop(Mult,Num(5),Num(4)));;
let funTest2 = Fun(varBool,TyBool,Bop(Equal,Num(5),Num(4)));;

let varTest = Var(varBool) ;;

let app = App(funTest,Num(4));; (* ela nao funcionou com o bop equal *)


Printf.printf "Verificando big step - bop Sum:" ;;
value2string (expBopSum)  ;;

print_newline();;

Printf.printf "Verificando big step - bop Equal: " ;;

value2string (expBopEqual) ;;
print_newline();;

Printf.printf "Verificando big step - if: " ;;
value2string (eval [] ifexp ) ;;
print_newline();;

Printf.printf "Verificando big step - if: " ;;
value2string (eval [] ifexpInt )  ;;
print_newline();;

Printf.printf "Verificando big step - fun: " ;;
value2string (eval [] funTest ) ;;
print_newline();;
Printf.printf "Verificando big step - fun: " ;;
value2string (eval [] funTest2 )  ;;
print_newline();;

Printf.printf "Verificando big step - var: " ;;
value2string (eval valueEnv2 varTest ) ;;
print_newline();;

Printf.printf "Verificando big step - app: ";;
value2string (eval [] app )  ;;
(* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *)






(* ** SSM2 Interpreter ** *)

(* ** SSM2 Compiler ** *)
