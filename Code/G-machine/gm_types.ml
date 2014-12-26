
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
(*	| Slide of int *)
	| Update of int
	| Pop of int
	;;

type gmCode = instruction list;;
type gmStack = addr list;;

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
	gmHeap *	(* heap of nodes *)
	gmGlobals *	(* global addresses in the heap *)
	gmStats	(* statistics *)
	);;

let getCode (code, _, _, _, _) = code;;
let putCode code (_, stack, heap, globals, stats) =
	(code, stack, heap, globals, stats);;

let getStack (_, stack, _, _, _) = stack;;
let putStack stack (code, _, heap, globals, stats) =
	(code, stack, heap, globals, stats);;

let getHeap (_, _, heap, _, _) = heap;;
let putHeap heap (code, stack, _, globals, stats) =
	(code, stack, heap, globals, stats);;

let getGlobals (_, _, _, globals, _) = globals;;
let putGlobals globals (code, stack, heap, _, stats) =
	(code, stack, heap, globals, stats);;

let statInitial = 0;;
let statIncSteps s = s + 1;;
let statGetSteps s = s;;

let getStats (_, _, _, _, stats) = stats;;
let putStats stats (code, stack, heap, globals, _) =
	(code, stack, heap, globals, stats);;
