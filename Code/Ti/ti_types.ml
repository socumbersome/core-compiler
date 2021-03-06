
open Core_types;;
open Core;;
open Heap;;

exception TiCompilationError of string;;
exception TiEvaluationError of string;;

type tiStack = addr list;;

type tiDump = DummyTiDump;;
let initialTiDump = DummyTiDump;;

type node = NAppl of addr * addr
	| NSupercomb of cName * cName list * coreExpr
	| NNum of int
	| NInd of addr (* indirection node *)
	;;

type tiHeap = node heap;;

(* associates each supercombinator name with the 
address of a heap node containing its definition *)
type tiGlobals = (cName * addr) list;;

(* records the number of steps taken *)
type tiStats = int;;

let tiStatInitial = 0;;
let tiStatIncSteps s = s + 1;;
let tiStatGetSteps s = s;;

type tiState = (tiStack * tiDump * tiHeap * tiStats);;

let applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
	= (stack, dump, heap, sc_defs, stats_fun stats);;

