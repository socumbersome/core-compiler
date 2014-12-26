
open Gm_types;;
open Heap;;

let gmFinal s = (getCode s) = [];;

let pushglobal f state =
	match (Lists.aLookup (getGlobals state) f) with
	| Some a -> putStack (a::getStack state) state
	| None -> raise (GmEvaluationError 
		("Undeclared global " ^ f));;

let pushint n state =
	let (heap', a) = hAlloc (getHeap state) (NNum n)
	in putHeap heap' (putStack (a::getStack state) state);;

let mkAppl state =
	let (a1::a2::ads') = getStack state
	in let (heap', a) = hAlloc (getHeap state) (NAppl(a1, a2))
	in putHeap heap' (putStack (a::ads') state);;

let getArg = function
	| NAppl(_, a2) -> a2
	| _ -> raise (GmEvaluationError 
		("Trying to get an argument of non-application node"));;

let push n state =
	let ads = getStack state
	in let a = getArg (hLookup (getHeap state) (List.nth ads (n+1)))
	in putStack (a::ads) state;;

let slide n state =
	let (a::ads) = getStack state
	in putStack(a::Lists.drop n ads) state;;

let unwind state =
	let heap = getHeap state
	in let (a::ads) = getStack state
	in let newState = function
		| NNum n -> state (* G-machine has terminated *)
		| NAppl(a1, a2) -> 
			putCode [Unwind] (putStack (a1::a::ads) state)
		| NGlobal(n, code) -> if List.length ads < n then
			raise (GmEvaluationError
				("Unwinding with too few arguments"))
			else putCode code state
	in newState (hLookup heap a);;

let dispatch i = match i with
	| Pushglobal f -> pushglobal f
	| Pushint n -> pushint n
	| MkAppl -> mkAppl
	| Push n -> push n
	| Slide n -> slide n
	| Unwind -> unwind;;

let step state =
	let (i::is) = getCode state
	in dispatch i (putCode is state);;

let doAdmin s = putStats (statIncSteps (getStats s)) s;;

(* eval : gmState -> gmState list *)
let rec eval state =
	let rest_states = 
		if gmFinal state then []
		else eval (doAdmin (step state))
	in state::rest_states;;

