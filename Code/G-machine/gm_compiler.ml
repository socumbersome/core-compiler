
open Core_types;;
open Core;;
open Gm_types;;
open Heap;;
open Miscellaneous;;

type gmCompiledSC = (cName * int * gmCode);;
type gmEnvironment = (cName, int) Lists.assoc;;
type gmCompiler = coreExpr -> gmEnvironment -> gmCode;;

let compiledPrimitives = [
	("+", 2, [Push 1; Eval; Push 1; Eval; Add; Update 2; Pop 2; Unwind]);
	("-", 2, [Push 1; Eval; Push 1; Eval; Sub; Update 2; Pop 2; Unwind]);
	("*", 2, [Push 1; Eval; Push 1; Eval; Mul; Update 2; Pop 2; Unwind]);
	("/", 2, [Push 1; Eval; Push 1; Eval; Div; Update 2; Pop 2; Unwind]);
	("neg", 1, [Push 0; Eval; Neg; Update 1; Pop 1; Unwind]);
	("==", 2, [Push 1; Eval; Push 1; Eval; Eq; Update 2; Pop 2; Unwind]);
	("!=", 2, [Push 1; Eval; Push 1; Eval; Ne; Update 2; Pop 2; Unwind]);
	("<", 2, [Push 1; Eval; Push 1; Eval; Lt; Update 2; Pop 2; Unwind]);
	("<=", 2, [Push 1; Eval; Push 1; Eval; Le; Update 2; Pop 2; Unwind]);
	(">", 2, [Push 1; Eval; Push 1; Eval; Gt; Update 2; Pop 2; Unwind]);
	(">=", 2, [Push 1; Eval; Push 1; Eval; Ge; Update 2; Pop 2; Unwind]);
	("if", 3, 
		[Push 0; Eval; Cond([Push 1], [Push 2]); Update 3; Pop 3; Unwind])
	];;

let argOffset n env = List.map (fun (v, m) -> (v, m + n)) env;;

let compileArgs defs env =
	let n = List.length defs
	in let nrevs = List.rev <| Lists.range 0 (n - 1)
	in Lists.zip (List.map fst defs) nrevs @ argOffset n env;;

let rec compileLet' defs env = match defs with
	| [] -> []
	| ((name, expr)::defsr) ->
		compileC expr env @ compileLet' defsr (argOffset 1 env)

(* comp is a compilation scheme for compiling expr *)
and compileLet comp defs expr env =
	let env' = compileArgs defs env
	in compileLet' defs env @ comp expr env'
		@ [ Slide (List.length defs) ]

and compileLetrec' defs env updno = match defs with
	| [] -> []
	| ((name, expr)::defsr) ->
		compileC expr env @ [Update updno] 
		@ compileLetrec' defsr env (updno - 1)

and compileLetrec comp defs expr env =
	let env' = compileArgs defs env
	in let n = List.length defs
	in [Alloc n] @ compileLetrec' defs env' (n - 1)
	@ comp expr env' @ [Slide n]

and compileC expr env = match expr with
	| EVar v -> (match Lists.aLookup env v with
		| Some n -> [Push n]
		| None -> [Pushglobal v]
		)
	| ENum n -> [Pushint n]
	| EAppl(e1, e2) -> compileC e2 env 
		@ compileC e1 (argOffset 1 env) @ [MkAppl]
	| ELet(isrec, defs, e) -> if isrec then
		compileLetrec compileC defs e env
		else compileLet compileC defs e env
	| ECase(e, alts) -> raise (GmCompilationError 
		("cannot compile case exprs yet"))
	| ELambd(vars, e) -> raise (GmCompilationError
		("cannot compile lambda abstractions yet"))
	;;

let compileR e env =
	let n = List.length env
	in compileC e env @ [Update n; Pop n; Unwind];;

let compileSc (name, varsn, body) =
	let n = List.length varsn
	in (name, n, compileR
		body 
		(Lists.zip varsn (Lists.range 0 (n - 1)))
	);;

let allocateSc heap (name, nargs, instrs) =
	let (heap', addr) = hAlloc heap (NGlobal(nargs, instrs))
	in (heap', (name, addr));;

let buildInitialHeap program =
	let compiled = List.map compileSc (program @ preludeDefs)
	in Lists.mapAccuml allocateSc hInitial
		(compiled @ compiledPrimitives);;

let initialCode = [Pushglobal "main"; Eval];;

let compile program =
	let (heap, globals) = buildInitialHeap program
	in (initialCode, [], [], heap, globals, statInitial);;
