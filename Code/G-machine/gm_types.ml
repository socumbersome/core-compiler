
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
	| Pack of int * int
	| Casejump of (int * gmCode) list
	| Split of int
	| Print
	| PrintEndStruct

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
	| NConstr of int * addr list (* structured data - int is a tag *)
	;;

let gmTrue = NConstr(2, []);;
let gmFalse = NConstr(1, []);;

(*let bool2gmBool b =
	if b then gmTrue else gmFalse;;*)

type gmHeap = node heap;;

type gmGlobals = (cName, addr) Lists.assoc;;

type gmStats = int;;

type gmOutput = string;;

type gmState = (
	gmOutput *	(* current output *)
	gmCode *	(* current instruction stream *)
	gmStack *	(* current stack *)
	gmDump *	(* current Dump *)
	gmHeap *	(* heap of nodes *)
	gmGlobals *	(* global addresses in the heap *)
	gmStats	(* statistics *)
	);;

let getOutput (out, _, _, _, _, _, _) = out;;
let putOutput out (_, code, stack, dump, heap, globals, stats) =
	(out, code, stack, dump, heap, globals, stats);;

let getCode (_, code, _, _, _, _, _) = code;;
let putCode code (out, _, stack, dump, heap, globals, stats) =
	(out, code, stack, dump, heap, globals, stats);;

let getStack (_, _, stack, _, _, _, _) = stack;;
let putStack stack (out, code, _, dump, heap, globals, stats) =
	(out, code, stack, dump, heap, globals, stats);;

let getDump (_, _, _, dump, _, _, _) = dump;;
let putDump dump (out, code, stack, _, heap, globals, stats) =
	(out, code, stack, dump, heap, globals, stats);;

let getHeap (_, _, _, _, heap, _, _) = heap;;
let putHeap heap (out, code, stack, dump, _, globals, stats) =
	(out, code, stack, dump, heap, globals, stats);;

let getGlobals (_, _, _, _, _, globals, _) = globals;;
let putGlobals globals (out, code, stack, dump, heap, _, stats) =
	(out, code, stack, dump, heap, globals, stats);;

let statInitial = 0;;
let statIncSteps s = s + 1;;
let statGetSteps s = s;;

let getStats (_, _, _, _, _, _, stats) = stats;;
let putStats stats (out, code, stack, dump, heap, globals, _) =
	(out, code, stack, dump, heap, globals, stats);;
