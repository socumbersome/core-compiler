
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

let compileArgs defs env =
	let nrevs = List.rev <| Lists.range 0 (n - 1)
	in let n = List.length defs
	in Lists.zip (List.map fst defs) nrevs @ argOffset n env;;

let rec compileLet' defs env = match defs with
	| [] -> []
	| ((name, expr)::defsr) ->
		compileC expr env @ compileLet' defs (argOffset 1 env);;

let compileLet comp defs expr env =
	let env' = compileArgs defs env
	in compileLet' defs env @ comp expr env'
		@ [ Slide (List.length defs) ];;

let rec compileC expr env = match expr with
	| EVar v -> (match Lists.aLookup env v with
		| Some n -> [Push n]
		| None -> [Pushglobal v]
		)
	| ENum n -> [Pushint n]
	| EAppl(e1, e2) -> compileC e2 env 
		@ compileC e1 (argOffset 1 env) @ [MkAppl]
	| ELet(isrec, defs, expr) -> if isrec then
		compileLetrec compileC defs expr env
		else compileLet compileC defs expr env
	| ECase(e, alts) -> raise (GmCompilationError 
		("cannot compile case exprs yet"))
	| ELambd(vars, e) -> raise (GmCompilationError
		("cannot compile lambda abstractions yet"))
	;;

let compileR e env =
	let n = List.length env
	in compileC e env @ [(*Slide (n + 1)*)Update n; Pop n; Unwind];;

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
