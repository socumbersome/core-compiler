
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

let unwind state =
	let heap = getHeap state
	in let (a::ads) as stack = getStack state
	in let newState = function
		| NNum n -> (match getDump state with
			| (code', s')::d' ->
				let state' = putStack (a::s') (putDump d' state)
				in putCode code' state'
			| [] -> state (* G-machine has terminated *)
			)
		| NAppl(a1, a2) -> 
			putCode [Unwind] (putStack (a1::a::ads) state)
		| NGlobal(n, code) -> 
			let k = List.length ads
			in if k < n then (match getDump state with
				| [] -> raise (GmEvaluationError
				("Unwinding with too few arguments and empty dump"))
				| (code', s')::d' ->
					let state' = putStack (Lists.last ads :: s') state
					in putCode code' (putDump d' state')
			)
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

let eval_instr state =
	let (a::ads) = getStack state
	in let code = getCode state
	in let s' = putDump ((code, ads)::getDump state) state
	in putCode [Unwind] (putStack [a] s');;

let boxInteger n state =
	let (h', a) = hAlloc (getHeap state) (NNum n)
	in putStack (a::getStack state) (putHeap h' state);;

let boxBoolean b state =
	let b' = if b then 1 else 0
	in let (h', a) = hAlloc (getHeap state) (NNum b')
	in putStack (a::getStack state) (putHeap h' state);;

let unboxInteger a state =
	let ub = function
		| NNum i -> i
		| _ -> raise (GmEvaluationError ("Unboxing a non-integer"))
	in ub (hLookup (getHeap state) a);;

(* primitive1 :
	(b -> gmState -> gmState) (* boxing function *)
	-> (addr -> gmState -> a) (* unboxing function *)
	-> (a -> b) (* unary operator *)
	-> (gmState -> gmState) (* state transition *)
*)
let primitive1 box unbox op state =
	let (a::ads) = getStack state
	in box (op (unbox a state)) (putStack ads state);;

let primitive2 box unbox op state =
	let (a0::a1::ads) = getStack state
	in box (op (unbox a0 state) (unbox a1 state))
		(putStack ads state);;

let arithmetic1 = primitive1 boxInteger unboxInteger;;

let arithmetic2 = primitive2 boxInteger unboxInteger;;

let comparison = primitive2 boxBoolean unboxInteger;;

let dispatchArith1 = function
	| Neg -> arithmetic1 (fun x -> -x)
	| _ -> raise (GmEvaluationError (
		"don't know other unary arith operators other than Neg"))
	;;

let dispatchArith2 = function
	| Add -> arithmetic2 (+)
	| Sub -> arithmetic2 (-)
	| Mul -> arithmetic2 ( * )
	| Div -> arithmetic2 (/)
	| _ -> raise (GmEvaluationError (
		"don't know other binary arith operators other than: "
		^ "Add, Sub, Mul, Div"))
	;;

let dispatchComparison = function
	| Eq -> comparison (=)
	| Ne -> comparison (fun x y -> not (x = y))
	| Lt -> comparison (<)
	| Le -> comparison (<=)
	| Gt -> comparison (>)
	| Ge -> comparison (>=)
	| _ -> raise (GmEvaluationError (
		"don't know other binary comparison operators other than: "
		^ "Eq, Ne, Lt, Le, Gt, Ge"))
	;;

let cond code1 code2 state =
	let (a::ads) = getStack state
	in match (hLookup (getHeap state) a) with
		| NNum 1 ->
			putCode (code1 @ getCode state) (putStack ads state)
		| NNum 0 ->
			putCode (code2 @ getCode state) (putStack ads state)
		| _ -> raise (GmEvaluationError (
		"Cond didn't find NNum 1 or NNum 0 on top of the stack"))
	;;

let dispatch i = match i with
	| Pushglobal f -> pushglobal f
	| Pushint n -> pushint n
	| MkAppl -> mkAppl
	| Push n -> push n
	| Slide n -> slide n
	| Update n -> update n
	| Pop n -> pop n
	| Unwind -> unwind
	| Alloc n -> alloc n
	| Eval -> eval_instr
	| ar1 when List.mem ar1 [Neg] -> dispatchArith1 ar1
	| ar2 when List.mem ar2 [Add; Sub; Mul; Div] -> 
		dispatchArith2 ar2
	| cp2 when List.mem cp2 [Eq; Ne; Lt; Le; Gt; Ge] ->
		dispatchComparison cp2
	| Cond(c1, c2) -> cond c1 c2
	;;

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

