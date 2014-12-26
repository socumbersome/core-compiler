
open Core_types;;
open Core;;
open Gm_types;;
open Heap;;

type gmCompiledSC = (cName * int * gmCode);;
type gmEnvironment = (cName, int) Lists.assoc;;
type gmCompiler = coreExpr -> gmEnvironment -> gmCode;;

let initialCode = [Pushglobal "main"; Unwind];;

let compiledPrimitives = [];;

let argOffset n env = List.map (fun (v, m) -> (v, m + n)) env;;

let rec compileC expr env = match expr with
	| EVar v -> (match Lists.aLookup env v with
		| Some n -> [Push n]
		| None -> [Pushglobal v]
		)
	| ENum n -> [Pushint n]
	| EAppl(e1, e2) -> compileC e2 env 
		@ compileC e1 (argOffset 1 env) @ [MkAppl]
	| _ -> raise (GmCompilationError ("cannot compile expressions "
		^ "other than var, num, appl now"));;

let compileR e env =
	let n = List.length env
	in compileC e env @ [Slide (n + 1); Unwind];;

let compileSc (name, env, body) =
	let n = List.length env
	in (name, n, compileR
		body 
		(Lists.zip env (Lists.range 0 (n - 1)))
	);;

let allocateSc heap (name, nargs, instrs) =
	let (heap', addr) = hAlloc heap (NGlobal(nargs, instrs))
	in (heap', (name, addr));;

let buildInitialHeap program =
	let compiled = List.map compileSc (program @ preludeDefs)
	in Lists.mapAccuml allocateSc hInitial compiled;;

let compile program =
	let (heap, globals) = buildInitialHeap program
	in (initialCode, [], heap, globals, statInitial);;
