RESULT = core_compiler
SOURCES = \
	./Utils/miscellaneous.ml ./Utils/lists.ml \
	./Utils/lazy_lists.ml ./Utils/heap.ml \
	./Utils/unique_names.ml \
	core_types.ml core.ml \
	core_AST_utils.ml core_case_lifter.ml \
	core_parser.mly core_lexer.mll \
	core_printer.ml \
	./Ti/ti_types.ml ./Ti/ti_compiler.ml \
	./Ti/ti_evaluator.ml ./Ti/ti_printer.ml \
	./G-machine/gm_types.ml ./G-machine/gm_printer.ml \
	./G-machine/gm_compiler.ml \
	./G-machine/gm_evaluator.ml  \
	core_compiler.ml

OCAMLMAKEFILE = OCamlMakefile
LIBS = str
include $(OCAMLMAKEFILE)
