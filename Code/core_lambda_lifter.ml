
open Core_types;;
open Core_AST_utils;;
open Unique_names;;
open Set;;

let freeVarsOf (free_vars, expr) = free_vars;;

let freeVarsOf_alt (tag, args, rhs) = CNameSet.(
	diff (freeVarsOf rhs) (of_list args);;

let freeVars_case lvars e alts = failwith "case not ympydet";;

(* freeVars_e : 
	cNameSet.t	(* candidates for free variables *) 
	-> cExpr	(* expression to annotate *)
	-> (cName * cNameSet.t) annExpr
*)
let rec freeVars_e lvars expr = match expr with
	| ENum n -> (CNameSet.empty, ANum n)
	| EVar v -> if CNameSet.mem v lvars then
		(CNameSet.singleton v, AVar v)
		else (CNameSet.empty, AVar v)
	| EAppl(e1, e2) ->
		let e1' = freeVars_e lvars e1
		in let e2' = freeVars_e lvars e2
		in (CNameSet.union (freeVarsOf e1') (freeVarsOf e2'),
				AAppl(e1', e2'))
	| ELambd(args, body) ->
		let new_lvars = CNameSet.(union lv (of_list args))
		in let body' = freeVars_e new_lv body
		in CNameSet.(
			(diff (freeVarsOf body') (of_list args),
			ALambd(args, body'))
		)
	| ELet(isrec, defns, body) ->
		CNameSet.(
		let binders = bindersOf defns
		in let binderSet = of_list binders
		in let body_lvars = union lvars binderSet
		in let rhs_lvars = if isrec then body_lvars
			else lvars
		in let rhss' = List.map (freeVars_e rhs_lvars)
			(rhssOf defns)
		in let defns' = Lists.zip binders rhss'
		in let freeInValues = of_list <| List.flatten <| 
			List.map freeVarsOf rhss'
		in let defnsFree = if isrec then
			diff freeInValues binderSet
			else freeInValues
		in let body' = freeVars_e body_lvars body
		in let bodyFree = diff (freeVarsOf body') binderSet
		in (union defnsFree bodyFree,
			ALet(isrec, defns', body'))
	| ECase(e, alts) -> freeVars_case lvars e alts
	| EConstr(tag, arity) -> failwith "constr not ymplemtd"
	;;


let freeVars prog = List.map (fun (name, args, body) ->
	(name, args, freeVars_e (CNameSet.of_list args) body)
	prog;;

let abstract_case free e alts = failwith "bbb";;

let rec abstract_e = function
	| (free, AVar v) -> EVar v
	| (free, ANum k) -> ENum k
	| (free, AAppl(e1, e2)) -> EAppl(abstract_e e1, abstract_e e2)
	| (free, ALet(isrec, defns, body) ->
		ELet(isrec, List.map 
			(fun (name, body) -> (name, abstract_e body))
			defns,
			abstract_e body)
	| (free, ALambd(args, body)) -> CNameSet.(
		let fvarsList = fold (fun el xs -> el::xs) free []
		in let sc_rhs = ELambd (fvarsList @ args) (abstract_e body)
		in let sc = ELet(nonRecursive, [("sc", sc_rhs)] (EVar "sc")
		in List.fold_left (fun e1 e2 -> EAppl(e1, e2)) sc
			(List.map (fun v -> EVar v) fvarsList)
	| (free, AConstr(tag, arity)) -> failwith "aa"
	| (free, ACase(e, alts)) -> abstract_case free e alts
	;;

let abstract prog = List.map (fun (sc_name, args, rhs) ->
	(sc_name, args, abstract_e rhs)) prog;;

(* ns stands for 'name supply' *)
let rec rename_e env ns = function
	| ENum n -> (ns, ENum n)
	| EAppl(e1, e2) ->
		let (ns1, e1') = rename_e env ns e1
		in let (ns2, e2') = rename_e env ns1 e2
		in (ns2, EAppl(e1', e2'))
	| ELambd(args, body) ->
		let (ns1, args', env') = newNames ns args
		in let (ns2, body') = rename_e (env' @ env) ns1 body
		in (ns1, ELambd(args', body'))
	| ELet(isrec, defns, body) ->
		let (ns1, body') = rename_e body_env (*TODO*)

let lambdaLift = collectSCs << rename << abstract << freeVars;;
