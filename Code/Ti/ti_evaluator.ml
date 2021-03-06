
open Ti_types;;
open Heap;;
open Core_types;;

let isDataNode = function
	| NNum _ -> true
	| _ -> false;;

let tiFinal (stack, dump, heap, globals, stats) = match stack with
	| [] -> raise (TiEvaluationError "Empty stack in tiFinal!")
	| [address] -> isDataNode (hLookup heap address)
	| _ -> false;;

let numStep state n = 
	raise (TiEvaluationError "Number applied as a function!");;

let applStep (stack, dump, heap, globals, stats) a1 a2 =
	(a1 :: stack, dump, heap, globals, stats);;

let instantiateConstr tag arity heap env =
	failwith "Can't instantiate constructors yet";;

let rec instantiateLet isrec defs body heap env =
	if isrec then (* magic here - firstly, allocating dummy objects
	and then using their addresses for real instantiations *)
		let (new_heap, naddresses) = Lists.mapAccuml
			(fun hp _ -> hAlloc hp (NNum 0) ) 
			heap			
			defs (* could be any list of length equal to the length of defs *)
		in let dummyenv = Lists.zip (Core.bindersOf defs) naddresses
		in let new_env = env @ dummyenv
		in let final_heap = List.fold_left
			(fun hp (body, addr) -> 
				let (h', a') = instantiate body hp new_env
				in let obj = hLookup h' a'
				in let h'' = hUpdate h' addr obj
				in let h''' = hFree h'' a'
				in h'''
			) 
			new_heap			
			(Lists.zip (Core.rhssOf defs) naddresses)
		in instantiate body final_heap new_env
	else
		let (new_heap, defsinst) = Lists.mapAccuml
			(fun hp body -> instantiate body hp env) 
			heap			
			(Core.rhssOf defs)
		in let new_env = env @ 
			(Lists.zip (Core.bindersOf defs) defsinst)
		in instantiate body new_heap new_env

(* instantiate:
	coreExpr (* body of supercombinator *)
	-> tiHeap (* heap before instantiation *)
	-> (cName * addr) list (* environment as association list *)
	-> (tiHeap, addr) (* heap after instantiation and an
		address of the root of an instance *)
*)
(*let rec*)and instantiate expr heap env = match expr with
	| ENum n -> hAlloc heap (NNum n)
	| EAppl(e1, e2) ->
		let (heap1, a1) = instantiate e1 heap env
		in let (heap2, a2) = instantiate e2 heap1 env
		in hAlloc heap2 (NAppl(a1, a2))
	| EVar v -> (try let address = List.assoc v env
		in (heap, address) 
		with 
			| Not_found -> raise (TiEvaluationError
			("Undefined name " ^ v))
		)
	| EConstr(tag, arity) ->
		instantiateConstr tag arity heap env
	| ELet(isrec, defs, body) ->
		instantiateLet isrec defs body heap env
	| ECase(_, _) -> failwith
		"Can't instantiate case expressions at all"
	| ELambd(_, _) -> failwith
		"Can't instantiate lambda abstractions (at all?)";;

(* instantiateAndUpdate:
	coreExpr (* body of supercombinator *)
	-> addr (* address of node to update *)
	-> tiHeap (* heap before instantiation *)
	-> (cName * addr) list (* environment as association list *)
	-> tiHeap (* heap after instantiation *)
*)
let instantiateAndUpdate expr upd_addr heap env = match expr with
	| ENum n -> failwith "aa"
	| EAppl(e1, e2) ->
		let (heap1, a1) = instantiate e1 heap env
		in let (heap2, a2) = instantiate e2 heap1 env
		in hUpdate heap2 upd_addr (NAppl(a1, a2))
	| EVar v -> failwith "aa"
	| EConstr(tag, arity) -> failwith "aa"
	| ELet(isrec, defs, body) -> failwith "aa"
	| ECase(_, _) -> failwith
		"Can't instantiate case expressions at all"
	| ELambd(_, _) -> failwith
		"Can't instantiate lambda abstractions (at all?)";;

(* TODO: make it return a lazy list! Or maybe not... *)
let getArgs heap = function
	| sc::stack ->
		let get_arg address =
			(match hLookup heap address with
				| NAppl(_, arg) -> arg
				| _ -> raise (TiEvaluationError 
				("Expected NAppl node but found different one "
				^ "in getArgs"))
			)
		(* in the paper there is just: List.map get_arg stack *)
		in (match hLookup heap sc with
			| NSupercomb(_, _, _) -> List.map get_arg stack
			| _ -> raise (TiEvaluationError
			("Expected a supercombinator node on top of stack "
			^ "but found different one in getArgs"))
		)
	| _ -> raise (TiEvaluationError "Empty stack in getArgs!");;

let scStep (stack, dump, heap, globals, stats)
	sc_name arg_names body =
	let args = getArgs heap stack
	in if List.(length arg_names > length args) then
		raise (TiEvaluationError ("Supercombinator " ^ sc_name ^
		" applied to too few arguments"))
	else
	let n = List.length arg_names
	in let arg_bindings = Lists.zip arg_names args(*(getArgs heap stack)*)
	in let env = arg_bindings @ globals
	in let (new_heap, result_addr) = instantiate body heap env
	in let addr_of_an = List.nth stack n
	in let new_stack = result_addr::(Lists.drop (n + 1) stack)
	in let final_heap = hUpdate new_heap addr_of_an (NInd result_addr)
	in (new_stack, dump, final_heap, globals, stats);;

let rec step ((stack, dump, heap, globals, stats) as state) =
	let dispatch = function
		| NNum n -> numStep state n
		| NAppl(a1, a2) -> applStep state a1 a2
		| NSupercomb(sc, args, body) -> scStep state sc args body
		| NInd a -> let new_stack = a::(List.tl stack)
			in step (new_stack, dump, heap, globals, stats)
	in dispatch (hLookup heap (List.hd stack));;

let doAdmin state = applyToStats tiStatIncSteps state;;

let rec eval state =
	let rest_states = 
		if tiFinal state then []
		else eval (doAdmin (step state))
	in state::rest_states;;

