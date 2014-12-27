
open Core_types;;
open Heap;;

exception GmCompilationError of string;;
exception GmEvaluationError of string;;

type instruction
	= Unwind
	| Pushglobal of cName
	| Pushint of int
	| Push of int
	| MkAppl
	| Slide of int
	| Update of int
	| Pop of int
	| Alloc of int
	| Eval	(* evaluates item on top of stack to WHNF *)
	| Add | Sub | Mul | Div | Neg
	| Eq | Ne | Lt | Le | Gt | Ge
	| Cond of gmCode * gmCode

and  gmCode = instruction list;;
type gmStack = addr list;;

type gmDumpItem = gmCode * gmStack;;
type gmDump = gmDumpItem list;;

type node
	= NNum of int
	| NAppl of addr * addr	(* applies the function at the
		first address to the expression at the second address *)
	| NGlobal of int * gmCode (* number of agruments and
		a code sequence to be executed when arguments are given *)
	| NInd of addr (* an indirection node *)
	;;

type gmHeap = node heap;;

type gmGlobals = (cName, addr) Lists.assoc;;

type gmStats = int;;

type gmState = (
	gmCode *	(* current instruction stream *)
	gmStack *	(* current stack *)
	gmDump *	(* current Dump *)
	gmHeap *	(* heap of nodes *)
	gmGlobals *	(* global addresses in the heap *)
	gmStats	(* statistics *)
	);;

let getCode (code, _, _, _, _, _) = code;;
let putCode code (_, stack, dump, heap, globals, stats) =
	(code, stack, dump, heap, globals, stats);;

let getStack (_, stack, _, _, _, _) = stack;;
let putStack stack (code, _, dump, heap, globals, stats) =
	(code, stack, dump, heap, globals, stats);;

let getDump (_, _, dump, _, _, _) = dump;;
let putDump dump (code, stack, _, heap, globals, stats) =
	(code, stack, dump, heap, globals, stats);;

let getHeap (_, _, _, heap, _, _) = heap;;
let putHeap heap (code, stack, dump, _, globals, stats) =
	(code, stack, dump, heap, globals, stats);;

let getGlobals (_, _, _, _, globals, _) = globals;;
let putGlobals globals (code, stack, dump, heap, _, stats) =
	(code, stack, dump, heap, globals, stats);;

let statInitial = 0;;
let statIncSteps s = s + 1;;
let statGetSteps s = s;;

let getStats (_, _, _, _, _, stats) = stats;;
let putStats stats (code, stack, dump, heap, globals, _) =
	(code, stack, dump, heap, globals, stats);;
