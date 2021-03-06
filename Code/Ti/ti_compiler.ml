
open Core_types;;
open Ti_types;;
open Core;;
open Heap;;

let extraPreludeDefs = [];;

let allocateSc heap (name, args, body) =
	let (heap', addr) = hAlloc heap (NSupercomb(name, args, body))
	in (heap', (name, addr));;

let buildInitialHeap sc_defs =
	Lists.mapAccuml allocateSc hInitial sc_defs;;

(* takes a program and creates the initial state of the
Template Instantiation machine *)
let compile program =
	let sc_defs = program @ preludeDefs @ extraPreludeDefs
	in let (initial_heap, globals) = buildInitialHeap sc_defs
	in let address_of_main = 
		(try List.assoc "main" globals with
			| Not_found -> 
				raise (TiCompilationError "main is not defined")
		)
	in let initial_stack = [address_of_main]
	in (initial_stack, initialTiDump, initial_heap, 
		globals, tiStatInitial);;


