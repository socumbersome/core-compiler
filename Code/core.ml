(**
An example program:

main = double 21;
double x = x + x

will be represented as:

[("main", [], (EAppl (EVar "double") (ENum 21)));
 ("double", ["x"], (EAppl (EAppl (EVar "+") (EVar "x")) (EVar "x")))
]
**)

open Core_types;;

(*
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
and
	cName = string
and
	cIsRec = bool
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

type 'a program = 'a supCombDef list;;
type coreProgram = cName program;;

let recursive:cIsRec = true;;
let nonRecursive:cIsRec = false;;
*)
let bindersOf defs = List.map fst defs;;
let rhssOf defs = List.map snd defs;;

let isAtomicExpr e = match e with
    | EVar _ -> true
    | ENum _ -> true
    | _ -> false;;

(**
Standard functions defined in the prelude:
I x = x
K x y = x
K1 x y = y
S f g x = f x (g x)
compose f g x = f (g x)
twice f = compose f f
**)
let preludeDefs =
    [("I", ["x"], EVar "x");
     ("K", ["x"; "y"], EVar "x");
     ("K1", ["x"; "y"], EVar "y");
     ("S", ["f"; "g"; "x"], 
        EAppl(
            (EAppl ((EVar "f"), (EVar "x"))),
            (EAppl ((EVar "g"), (EVar "x")))
        )
     );
     ("compose", ["f"; "g"; "x"],
        EAppl(
            (EVar "f"),
            (EAppl ((EVar "g"), (EVar "x")))
        )
     );
     ("twice", ["f"],
        EAppl(
            (EAppl ((EVar "compose"), (EVar "f"))),
            (EVar "f")
        )
     );
	("cons", [], EConstr(2, 2));
	("nil", [], EConstr(1, 0))
    ];;


