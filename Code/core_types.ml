type cName = string;;
type cIsRec = bool;;
type 'a cExpr =
	  EVar of cName (* could be "x" as well as "+"! *)
	| ENum of int
	| EConstr of int * int (* EConstr(tag, arity) *)
	| EAppl of 'a cExpr * 'a cExpr (* function application *)
	| ELet of 
		cIsRec	(* bool with True iff recursive *)
		* ('a * 'a cExpr) list (* definitions *)
		* 'a cExpr (* body of let[rec] *)
	| ECase of 'a cExpr * 'a cAlter list
	| ELambd of 'a list * 'a cExpr (* lambda abstractions *)
(*and
	cName = string
and
	cIsRec = bool*)
and
	'a cAlter = int * 'a list * 'a cExpr
	;;

type coreExpr = cName cExpr;;
type coreAlter = cName cAlter;;

(* a `function` f is a supercombinator iff 1) or 2), where
1) it is constant
2) it has no free variables and every inner function
(or, lambda abstraction) in the body of f is
also a supercombinator *)
type 'a supCombDef = cName * 'a list * 'a cExpr;;
type coreSupCombDef = cName supCombDef;;

type 'a program = ('a supCombDef) list;;
type coreProgram = cName program;;

let recursive:cIsRec = true;;
let nonRecursive:cIsRec = false;;
