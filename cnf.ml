(* Convert a Boolean Formula to CNF form *)

open Ast
open ExpressionLibrary

exception UnExpectedExpr of string
;;

(* Dual of operartor; used in De'Morgans law *)
let dual (op : binop) : binop =
  match op with
	| And -> Or
	| Or -> And
	| _ -> raise (UnExpectedExpr "Unexpected operator")
;;

(* Distribution Law *)
let rec distribute (unit : expression) (form : expression) : expression =
  match form with
	| Var v -> Binop (Or, unit, form)
	| Unop (_, _) -> Binop (Or, unit, form)
	| Binop (op, p, q) -> (
	  match op with
		| Or -> Binop(Or, unit, form)
		| And -> Binop(And, (distribute unit p), (distribute unit q))
		| _ -> raise (UnExpectedExpr ("unexpected expression: Expected only Or or And"))
		)
;;

let counter = ref (-1) ;;

let newvar (arg : int) : expression = 
  Var (Ident.create ("xxxx" ^ (string_of_int (counter := arg + 1; !counter))));;

let convert (expr : expression) : expression =
	  let rec cnf (exp : expression) : expression = 
		match exp with
		  | Var va -> Var va
		  | Unop (Neg, p) -> pushdownNeg (Unop (Neg, (cnf p)))
		  | Binop (op, p, q) -> 
			match op with 
			  | Or -> (
				match (p, q) with
				  | (Var v1, Var v2) -> Binop(Or, p, q)
				  | (Unop(op1, Var v1), Unop(op2, Var v2)) -> Binop (Or, p, q)
				  | (Unop(op1, Var v1), f2) -> distribute p (cnf f2)
				  | (f1, Unop(op2, Var v2)) -> distribute q (cnf f1)
				  | (Var v1, _) -> distribute p (cnf q)
				  | (_, Var v2) -> distribute q (cnf p)
				  | (Unop(_, p1), q1) -> cnf (Binop (Or, (pushdownNeg (Unop (Neg, (cnf p1)))), cnf q1))
				  | (p1, Unop(_, q1)) -> cnf (Binop (Or, cnf p1, pushdownNeg (Unop (Neg, (cnf p1)))))
				  (* | (Unop(_,p1), Unop(_,q1)) -> cnf (Binop(Or, (pushdownNeg (Unop (Neg, (cnf p1)))), *)
				  (* 										 (pushdownNeg (Unop (Neg, (cnf q1)))))) *)
				  | (Binop (And, f11, f12), Binop (And, f21, f22)) -> 
					let nv = newvar (!counter) in
					cnf (Binop (And, 
							(Binop(Imply, nv, p)), 
							(Binop(Imply, Unop(Neg, nv), q))
					     ))
				  |(Binop (Or, f11, f12), Binop (_, f21, f22)) ->
					cnf (Binop (Or, cnf f11, cnf (Binop (Or, cnf f12, cnf q)))) 
				  |(Binop (_, f11, f12), Binop (Or, f21, f22)) ->
					cnf (Binop (Or, cnf (Binop (Or, cnf p, cnf f21)), cnf f22)) 
				  |(Binop (_, f11, f12), Binop (_, f21, f22)) ->
					cnf (Binop (Or, cnf p, cnf q))
				  (* | (_, _) -> cnf (Binop (Or, p, q)) *)
			  )
			  | And -> Binop(And, (cnf p), (cnf q))
			  | Xor -> cnf (Binop(Or, 
								  (Binop (And, p, (Unop(Neg, q)))), 
								  (Binop (And, (Unop(Neg, p)), q))))
			  | Imply -> cnf (Binop(Or, cnf (Unop(Neg, p)), (cnf q)))
			  | Iff -> cnf (Binop(And, 
								  (Binop (Imply, p, q)), 
								  (Binop (Imply, q, p))))
	  and pushdownNeg (exp : expression) : expression =
		match exp with
		  | Unop (Neg, p) -> 
			(
			  match p with
				| Var v -> Unop(Neg, p)
				| Unop (Neg, Var v) -> Var v
				| Unop (Neg, q) -> cnf q
				| Binop (op, x, y) ->  cnf (Binop( (dual op), Unop(Neg, x), Unop(Neg, y)))
			)
		   | _ -> raise (UnExpectedExpr ("Unexpected expression: Expected only Unary operator"))
	  in cnf expr
;;
