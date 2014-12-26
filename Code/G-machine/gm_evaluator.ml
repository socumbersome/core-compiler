
open Gm_types;;
open Heap;;
open Miscellaneous;;

let gmFinal s = (getCode s) = [];;

let pushglobal f state =
	match (Lists.aLookup (getGlobals state) f) with
	| Some a -> putStack (a::getStack state) state
	| None -> raise (GmEvaluationError 
		("Undeclared global " ^ f));;

let pushint n state =
	match (Lists.aLookup (getGlobals state) (string_of_int n)) with
		| Some a -> putStack (a::getStack state) state
		| None -> 
			let (heap', a) = hAlloc (getHeap state) (NNum n)
			in let state' = putGlobals 
				((string_of_int n, a)::getGlobals state) state
			in putHeap heap' (putStack (a::getStack state') state')
	;;

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
	in let an = List.nth ads n
	in putStack (an::ads) state;;

let slide n state =
	let (a::ads) = getStack state
	in putStack (a::Lists.drop n ads) state;;

let update n state = 
	let (a::ads) = getStack state
	in let stack' = ads
	in let an = List.nth ads n
	in let heap' = hUpdate (getHeap state) an (NInd a)
	in putHeap heap' (putStack stack' state);;

let pop n state =
	putStack (Lists.drop n (getStack state)) state;;

let rearrange n heap stack =
	let lstack' = Lazy_lists.tolazy <| 
		List.map (getArg << (hLookup heap)) (List.tl stack)
	in Lazy_lists.ltake (n, lstack') @ Lists.drop n stack;;

let rec unwind state =
	let heap = getHeap state
	in let (a::ads) as stack = getStack state
	in let newState = function
		| NNum n -> state (* G-machine has terminated *)
		| NAppl(a1, a2) -> 
			putCode [Unwind] (putStack (a1::a::ads) state)
		| NGlobal(n, code) -> if List.length ads < n then
			raise (GmEvaluationError
				("Unwinding with too few arguments"))
			else 
				let stack' = rearrange n heap stack
				in putCode code (putStack stack' state)
		| NInd ia -> putCode [Unwind] (putStack (ia::ads) state)
	in newState (hLookup heap a);;

let rec allocNodes n heap = if n = 0 then (heap, []) else
	let (heap1, ads) = allocNodes (n - 1) heap
	in let (heap2, a) = hAlloc heap1 (NInd hNull)
	in (heap2, a::ads);;

let alloc n state =
	let (heap', ads) = allocNodes n (getHeap state)
	in let state' = putHeap heap' state
	in let stack = getStack state'
	in putStack (ads @ stack) state';;

let dispatch i = match i with
	| Pushglobal f -> pushglobal f
	| Pushint n -> pushint n
	| MkAppl -> mkAppl
	| Push n -> push n
	| Slide n -> slide n
	| Update n -> update n
	| Pop n -> pop n
	| Unwind -> unwind
	| Alloc n -> alloc n;;

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

