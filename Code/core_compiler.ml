(*
let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      Core_parser.input Core_lexer.token lexbuf
    done
  with End_of_file -> exit 0
      
let _ = Printexc.print main ()
*)

let main () =
	let cin = open_in Sys.argv.(1)
	in let lexbuf = Lexing.from_channel cin
	in let program = Core_parser.program Core_lexer.token lexbuf
	in let compiledToTi = Ti_compiler.compile program
	in let tirun = Ti_evaluator.eval compiledToTi
	in let res = Ti_printer.showResults tirun
	in print_string res
	(*(Core_printer.pprint program)*);;

let _ = main ();;
