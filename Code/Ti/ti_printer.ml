open Ti_types;;
open Ti_evaluator;;
open Core_printer;;
open Miscellaneous;;

let addr2iseq address = iStr (Heap.showaddr address);;

(* FW - fixed width *)
let showFWAddr address =
	let str = Heap.showaddr address
	in iStr ((spaces (4 - String.length str)) ^ str);;

let showNode = function
	| NAppl(a1, a2) -> iConcat [ iStr "NAppl ";
		addr2iseq a1; iStr " "; addr2iseq a2 ]
	| NSupercomb(name, args, body) -> 
		iStr ("NSupercomb " ^ name)
	| NNum n -> iConcat [ iStr "NNum "; iNum n ]
	| NInd a -> iConcat [ iStr "NInd "; addr2iseq a ];;

let showStkNode heap = function
	| NAppl(fun_addr, arg_addr) -> iConcat 
		[ iStr "NAppl "; showFWAddr fun_addr; iStr " ";
		showFWAddr arg_addr; iStr " (";
		showNode (Heap.hLookup heap arg_addr); iStr ")" ]
	| node -> showNode node;;

let showStack heap stack =
	let show_stack_item address =
		iConcat [ showFWAddr address; iStr ": ";
			showStkNode heap (Heap.hLookup heap address) ]
	in iConcat [ iStr "Stk [";
	iIndent (iInterleave iNewline (List.map show_stack_item stack));
	iStr " ]" ];;

let showState (stack, dump, heap, globals, stats)
	= iConcat [ showStack heap stack; iNewline ];;

let showStats (stack, dump, heap, globals, stats) =
	iConcat [ iNewline; iNewline;
	iStr "Total number of steps = ";
	iNum (tiStatGetSteps stats) ];;

let showResults states =
	iDisplay (iConcat [ iLayn (List.map showState states);
		showStats (Lists.last states) ]);;
