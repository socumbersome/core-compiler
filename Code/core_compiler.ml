let print_core_raw program =
	let printed = Core_printer.pprint program
	in print_string printed;;

let print_core_caselifted program =
	let lifted = Core_case_lifter.liftProgram program
	in print_string (Core_printer.pprint lifted);;

let print_compiled_to_g program =
	let compiledToGm = Gm_compiler.compile program
	in print_string (Gm_printer.showResults [compiledToGm]);;

let to_g_and_interpret program =
	let lifted = Core_case_lifter.liftProgram program
	in let compiledToGm = Gm_compiler.compile lifted
	in let gmrun = Gm_evaluator.eval compiledToGm
	in print_string (Gm_printer.showResults gmrun)

let main () =
	let cin = open_in Sys.argv.(1)
	in let lexbuf = Lexing.from_channel cin
	in let program = Core_parser.program Core_lexer.token lexbuf
	in to_g_and_interpret program;;
	(*in let compiledToGm = Gm_compiler.compile program
	in let gmrun = Gm_evaluator.eval compiledToGm
	in let res = Gm_printer.showResults gmrun
	in print_string res*)
	(*Core_printer.pprint program*)

let _ = main ();;
