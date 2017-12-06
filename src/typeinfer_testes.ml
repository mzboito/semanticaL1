#use "term_L1.ml" ;;
#use "typeinfer_L1.ml" ;;
#use "utils.ml" ;;

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
let currentEnv = updateExpr (t21) (Num(3)) (env);;

let t24 = "variavelBool" ;;
let currentEnv2 = updateExpr (t24) (Bool(true)) (env) ;;

let t22 = Fun(t21,TyInt,t15);;
let t23 = Fun(t21,TyInt,t5);;

let evalList = t1:: (t2:: (t3 :: [])) ;;
let output = List.map (fun x -> (typecheck [] x)) evalList ;;
let strings = List.map (fun t -> (type2string t)) output ;;

(* e1: t -> t'   e2: t    e1e2: t' *)

let t25 = App(t22,t1) ;; (* t22: int -> int   t1: int    e1e2: int *)
let t27 = App(t23,t14) ;; (* t23:  int -> bool   e2: int    e1e2: bool , essa da erro *)
let t26 = App(t23,t9) ;; (* t23: int -> bool   e2: bool    e1e2: bool , essa da erro ok *) 

(* e1: T  vaar: T  e2: T', let é T' *)

let t28 = Let(t24,TyBool, t19, t16) ;;
let t29 = Let(t21,TyInt, t16, t19) ;;
let t30 = Let(t21,TyInt, t19, t16) ;; (* deve dar erro *)

(* Testes para let *)

Printf.printf "Verificando tipo let: %s" (type2string (typecheck (currentEnv2) (t28))) ;; (* currentEnv2 porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;

Printf.printf "Verificando tipo let: %s" (type2string (typecheck (currentEnv) (t29))) ;; (* currentEnv2 porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;

Printf.printf "Verificando tipo let: %s" (type2string (typecheck (currentEnv) (t30))) ;; (* currentEnv2 porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;


(* Testes para app *)

Printf.printf "Verificando tipo app: %s" (type2string (typecheck (currentEnv) (t25))) ;; (* currentEnv porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;

Printf.printf "Verificando tipo app: %s" (type2string (typecheck (currentEnv) (t27))) ;; (* currentEnv porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;

Printf.printf "Verificando tipo app: %s" (type2string (typecheck (currentEnv) (t26))) ;; (* currentEnv porque a variável usada em fun foi definida nesse ambiente! *)
print_newline();;

(* Testes para tipo Num, Bool, Bop - Ok *)

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

(* Testes para o if - ok *)

print_newline();;
Printf.printf "Verificando if: %s" (type2string (typecheck [] t18)) ;;
print_newline();;
Printf.printf "Verificando if: %s" (type2string (typecheck [] t19)) ;;
print_newline();;
Printf.printf "Verificando if: %s" (type2string (typecheck [] t20)) ;;
print_newline();;

(* Testes para o fun - ok *)
Printf.printf "Verificando fun: %s" (type2string (typecheck currentEnv t22)) ;; (* variavel t21 definida dentro do currentenv*)
print_newline();;  
Printf.printf "Verificando fun: %s" (type2string (typecheck currentEnv t23)) ;; 
print_newline();;  

(* Testes para a var - ok *)
Printf.printf "Verificando var: %s" (type2string (typecheck (currentEnv) (Var(t21))) ) ;; 
print_newline();;  
Printf.printf "Verificando var: %s" (type2string (typecheck (currentEnv2) (Var(t24))) ) ;; 
print_newline();;  


