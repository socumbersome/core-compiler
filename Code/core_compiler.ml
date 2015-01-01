open Miscellaneous;;

let printCore =
	print_string << Core_printer.pprint

let caseLift =
	Core_case_lifter.liftProgram;;

let lambdaLift =
	Core_lambda_lifter.lambdaLift;;

let lift = lambdaLift >> caseLift;;

let compile2Gm = Gm_compiler.compile;;

let interpretGmAllStates = Gm_evaluator.eval;;

let interpretGmLastState = Gm_evaluator.eval_only_last;;

let printGmAll states = print_string << Gm_printer.showResults <| states;;

let printGmLast = print_string << Gm_printer.showResult;;

let printLifted = lift >> printCore;;

let toGmAndPrintAll = lift >> compile2Gm >> interpretGmAllStates >> printGmAll;;

let toGmAndPrintLast = lift >> compile2Gm >> interpretGmLastState >> printGmLast;;

let main () =
	let cin = open_in Sys.argv.(1)
	in let lexbuf = Lexing.from_channel cin
	in let program = Core_parser.program Core_lexer.token lexbuf
	in toGmAndPrintLast <| program;;

let _ = main ();;
