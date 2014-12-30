
open Core_types;;
open Core;;
open Gm_types;;
open Heap;;
open Core_AST_utils;;
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

let builtInBinary = [ ("+", Add); ("-", Sub); ("*", Mul);
	("/", Div); ("==", Eq); ("!=", Ne); ("<", Lt);
	("<=", Le); (">", Gt); (">=", Ge) ];;

let argOffset n env = List.map (fun (v, m) -> (v, m + n)) env;;

let rec compileExprAscEnv comp exs env = match exs with
	| [] -> []
	| e::es -> 
		comp e env @ compileExprAscEnv comp es (argOffset 1 env)
	;;

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

(* compile in non-strict context - C scheme *)
and compileC expr env = match expr with
(*	| EVar "true" -> [Pushglobal "true"]
	| EVar "false" -> [Pushglobal "false"] *)
	| EVar v -> (match Lists.aLookup env v with
		| Some n -> [Push n]
		| None -> [Pushglobal v]
		)
	| ENum n -> [Pushint n]
	| satc when isSaturatedConstr satc ->
	(* I fear the idea of looking for EConstr is really bad... *)
		let (EConstr(tag, arity), args) = dismantleConstr satc
		in compileExprAscEnv compileC (List.rev args) env
			@ [Pack(tag, arity)]
	| EAppl(e1, e2) -> compileC e2 env 
		@ compileC e1 (argOffset 1 env) @ [MkAppl]
	| ELet(isrec, defs, e) -> if isrec then
		compileLetrec compileC defs e env
		else compileLet compileC defs e env
	| ECase(e, alts) -> raise (GmCompilationError 
		("cannot compile case exprs in lazy ctxt yet"))
	| ELambd(vars, e) -> raise (GmCompilationError
		("cannot compile lambda abstractions yet"))
	| EConstr(t, a) -> [Pushglobal ("Pack{" ^ string_of_int t
		^ "," ^ string_of_int a ^ "}") ]
	;;

(* D scheme compilation; comp should correspond to A scheme *)
let compileAlts comp alts env =
	List.map (fun (tag, names, body) -> 
		let n = List.length names
		in (tag, comp n body (Lists.zip names (Lists.range 0 (n - 1))
			@ argOffset n env))
	) alts;;

(* compile in strict context - E scheme 
 (that means, when run, it will evaluate
 expr to WHNF) *)
let rec compileE expr env = match expr with
	| ENum n -> [Pushint n]
	| ELet(isrec, defs, e) -> if isrec then
		compileLetrec compileE defs e env
		else compileLet compileE defs e env
	| EAppl(EAppl(EVar op, e1), e2) when 
		List.mem op (Lists.aDomain builtInBinary) ->
		let Some ibin = Lists.aLookup builtInBinary op
		in compileE e2 env @ compileE e1 (argOffset 1 env) @ [ibin]
	| EAppl(EVar "neg", e) ->
		compileE e env @ [Neg]
	| EAppl(EAppl(EAppl(EVar "if", e0), e1), e2) ->
		compileE e0 env 
		@ [Cond(compileE e1 env, compileE e2 env)]
	| ECase(e, alts) -> compileE e env
		@ [Casejump (compileAlts compileE' alts env)]
	| satc when isSaturatedConstr satc ->
	(* mind you - the same code as in compileC ! *)
		let (EConstr(tag, arity), args) = dismantleConstr satc
		in compileExprAscEnv compileC (List.rev args) env
			@ [Pack(tag, arity)]
	| _ -> compileC expr env @ [Eval]

(* plays role of A scheme compilation *)
and compileE' offset expr env =
	[Split offset] @ compileE expr env @ [Slide offset];;

let compileR e env =
	let n = List.length env
	in compileE e env @ [Update n; Pop n; Unwind];;

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

(** DEPRECATED *)
let allocateAtomicValues heap globals =
	let (h', truea) = hAlloc heap gmTrue
	in let (h'', falsea) = hAlloc h' gmFalse
	in (h'', ("true", truea)::("false", falsea)::globals)

let initialCode = [Pushglobal "main"; Eval; Print];;

let compile program =
	let (heap, globals) = buildInitialHeap program
	(*in let (h', g') = allocateAtomicValues heap globals*)
	in ("", initialCode, [], [], heap, globals, statInitial);;
