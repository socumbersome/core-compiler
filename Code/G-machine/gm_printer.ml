
open Gm_types;;
open Core_printer;;
open Heap;;
open Miscellaneous;;

let showInstruction = function
	| Unwind -> iStr "Unwind"
	| Pushglobal f -> iConcat [ iStr "Pushglobal "; iStr f ]
	| Push n -> iConcat [ iStr "Push "; iNum n ]
	| Pushint n -> iConcat [ iStr "Pushint "; iNum n ]
	| MkAppl -> iStr "MkAppl"
(*	| Slide n -> iConcat [ iStr "Slide "; iNum n ];; *)
	| Update n -> iConcat [ iStr "Update "; iNum n ]
	| Pop n -> iConcat [ iStr "Pop "; iNum n ];;

let showInstructions code =
	iConcat [ iStr " Code:{";
		iIndent (iInterleave iNewline
			(List.map showInstruction code));
		iStr "}"; iNewline ];;

let showSC s (name, addr) =
	let NGlobal(arity, code) = hLookup (getHeap s) addr
	in iConcat [ iStr "Code for "; iStr name; iNewline;
		showInstructions code; iNewline; iNewline ];;

let showNode s a = function
	| NNum n -> iNum n
	| NGlobal(n, g) -> 
		let v = (List.hd << List.map fst) (List.filter 
			(fun (_, b) -> a = b)
			(getGlobals s)
		)
		in iConcat [ iStr "Global "; iStr v]
	| NAppl(a1, a2) -> iConcat [ iStr "Appl "; iStr (showaddr a1);
		iStr " "; iStr (showaddr a2) ]
	| NInd a -> iConcat [ iStr "Ind "; iStr (showaddr a) ];;

let showStackItem s a =
	iConcat [ iStr (showaddr a); iStr ": ";
		showNode s a (hLookup (getHeap s) a) ];;

let showStack s =
	iConcat [ iStr " Stack:["; iIndent (
		iInterleave iNewline
			(List.map (showStackItem s) (List.rev (getStack s))));
		iStr "]" ];;

let showState s = iConcat [ showStack s; iNewline;
	showInstructions (getCode s); iNewline ];;

let showStats s = iConcat [ iStr "Steps taken = "; 
	iNum (statGetSteps (getStats s)) ];;

let showResults states = 
	let s::ss = states
	in iDisplay (iConcat [
		iStr "Supercombinator definitions"; iNewline;
		iInterleave iNewline (List.map (showSC s) (getGlobals s));
		iNewline; iNewline; iStr "State transitions"; iNewline;
		iNewline; iLayn (List.map showState states); iNewline;
		iNewline; showStats (Lists.last states) ]
	);;
