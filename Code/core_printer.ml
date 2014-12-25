(**
A "ppr" prefix in functions' names means "pretty-print"
*)

open Core;;
open Core_types;;
open Lists;;
open Lazy_lists;;
open Miscellaneous;;

type iseq = INil 
	| IStr of string
	| IAppend of iseq * iseq
	| IIndent of iseq
	| INewline;;

let iNil = INil;;
let iAppend seq1 seq2 = 
	if seq1 = iNil then seq2 else
	if seq2 = iNil then seq1 else
	IAppend(seq1, seq2);;

(* need to take care of \n in str !? *)
let iStr str = IStr str;;
let iIndent seq = IIndent seq;;
let iNewline = INewline (*iStr "\n"*);;
let iNum n = iStr (string_of_int n);;
let iFWNum width n =
	let digits = string_of_int n
	in iStr ((*(String.make 
		(width - String.length digits) ' '*)
		(spaces (width - String.length digits)) ^ digits
	);;
let iConcat seqs = List.fold_left iAppend iNil seqs;;
let iLayn seqs =
	let lay_item (n, seq) =
		iConcat [ iFWNum 4 n; iStr ") ";
			iIndent seq; iNewline]
	in let litems = lmap lay_item (lzip (lfrom 1) (tolazy seqs))
	in iConcat (fromlazy litems);;

let rec iInterleave seq seqs = match seqs with
	| [s] -> s
	| s1::s2::ss -> 
		iConcat [ s1; seq; iInterleave seq (s2::ss) ]
		(*iAppend
			(iAppend (iAppend s1 seq) s2)
			(iInterleave seq ss)*)
	| [] -> iNil;;

(*let printProgram program = "";;

let rec printcExpr expr = match expr with
    | ENum n -> string_of_int n
    | EVar x -> x
    | EAppl(e1, e2) -> printcExpr e1 ^ " " ^ printcExpr e2
    | _ -> "how?:(";;

let printcExprWithParenths expr =
    if isAtomicExpr expr then printcExpr expr
    else "(" ^ printcExpr expr ^ ")";;
*)
(** generates e1 e2 e2 ... e2 (e2 appears exactly n times) *)
let mkMultiAppl n e1 e2 = 
	List.fold_left (fun x y -> EAppl(x, y)) e1 (Lists.buildn n e2);;

(** flattenseq : Int (* current column; 0 for first column *)
	-> (iseq * int) list (* work list *)
	-> string (* result *)
*)
let rec flattenseq col seqs = match seqs with
	| [] -> ""
	| x::xs -> (match x with
		| (INil, _) -> flattenseq col xs
		| (IStr s, _) -> s ^ (flattenseq (col + String.length s) xs)
		| (IAppend(s1, s2), indent) -> 
			flattenseq col ((s1, indent)::(s2, indent)::xs)
			(*match s1 with
				| (IIndent seq, indent) ->
					let back = (iIndent iNil, 
					flattenseq (col + 4) (
			*)
		| (IIndent seq, _) -> 
			flattenseq col ((seq, col)::xs) 
		| (INewline, indent) -> "\n" ^ spaces indent(*(String.make indent ' ')*) ^
			(flattenseq indent xs)
	);;

let iDisplay seq = flattenseq 0 [(seq, 0)];;

let pprVars vars = iInterleave (iStr " ")
	(List.map iStr vars);;

let rec pprExpr = function
	| ENum n -> iStr (string_of_int n)
	| EVar v -> iStr v
	| EAppl(EAppl(EVar op, e1), e2) when 
		List.mem op ["+"; "-"; "*"; "/"; "<"; ">"; "<="; ">="]
		-> iConcat [pprAExpr e1; iStr (" " ^ op ^ " "); 
		pprAExpr e2]
	| EAppl(e1, e2) ->
		iConcat [ pprExpr e1; iStr " "; pprAExpr e2 ]
	| ELet(isrec, defns, expr) ->
		let keyword = if isrec then "letrec" else "let"
		in iConcat [ iStr keyword; iNewline;
			iStr "  "; iIndent (pprDefns defns);
			iNewline; iStr "in ";
			pprExpr expr ]
	| EConstr(a, b) ->
		iConcat [ iStr "Constructor{";
			iStr (string_of_int a);
			iStr ",";
			iStr (string_of_int b);
			iStr "}" ]
	| ECase(e1, alts) -> iConcat [ iStr "case ";
		pprExpr e1; iStr " of"; iNewline;
		iStr "  "; iIndent (pprAlts alts) ]
	| ELambd(vars, expr) ->
		iConcat [ iStr "\\ "; pprVars vars;
		iStr ". "; pprExpr expr ]

and pprAlt (tag, vars, expr) =
	iConcat [ iStr "#"; iStr (string_of_int tag);
		iStr "# "; pprVars vars; iStr "-> ";
		(*iNewline;*) iIndent (pprExpr expr)
		(*pprExpr expr*) ]

and pprAlts alts =
	let sep = iConcat [iStr ";"; iNewline ]
	in iInterleave sep (List.map pprAlt alts)

and pprDefn (name, expr) =
	iConcat [ iStr name; iStr " = ";
		iIndent (pprExpr expr) ]

and pprDefns defns = 
	let sep = iConcat [ iStr ";"; iNewline ]
	in iInterleave sep (List.map pprDefn defns)

(* "A" from "Atomic vs not atomic" *)
and pprAExpr expr = let pexpr = pprExpr expr
	in if isAtomicExpr expr then pexpr
	else iConcat [ iStr "("; pexpr; iStr ")" ];;

let pprSupComb (name, vars, expr) =
	(*let pprvars = iInterleave (iStr " ") 
		(List.map (fun v -> EVar v) vars)*)
	iConcat [ (*iStr "var"; iStr " ";*) iStr name;
		iStr " "; pprVars vars;
		iStr "= "; pprExpr expr ];;

let pprProgram prog =
	let sep = iConcat [ iStr ";"; iNewline ]
	in iInterleave sep (List.map pprSupComb prog);;

let pprint prog = iDisplay (pprProgram prog);;

(** tests *)
let exp1 = ELet(
	false, 
	[("x", ELet(
		true,
		[("cos", EAppl(EAppl(EVar "&", EVar "x"), EVar "y"));
		("ok", EAppl(EAppl(EVar "+", EVar "x"), EVar "y"))],
		EVar "bla"
	)); ("z", ENum 3)],
	ECase(
		EVar "misio",
		[(6, ["t"; "w"], EVar "i")]
	)
	);;
let exp2 = EVar "nowa zmienna";;
let prog = [("jeden", [], exp1); ("dwa", [], exp2)];;
