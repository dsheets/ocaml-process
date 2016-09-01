.PHONY: build test install uninstall reinstall clean

FINDLIB_NAME=process
MOD_NAME=process

OCAMLBUILD=ocamlbuild -use-ocamlfind -classic-display

TARGETS=.cma .cmxa .cmx

PRODUCTS=$(addprefix $(MOD_NAME),$(TARGETS))

TYPES=.mli .cmi .cmti

INSTALL:=$(addprefix $(MOD_NAME), $(TYPES)) \
         $(addprefix $(MOD_NAME), $(TARGETS))

INSTALL:=$(addprefix _build/lib/,$(INSTALL))

ARCHIVES:=_build/lib/$(MOD_NAME).a

build:
	$(OCAMLBUILD) $(PRODUCTS)

test_%.native: lib_test/%.ml
	$(OCAMLBUILD) lib_test/$*.native
	mv $*.native test_$*.native

TEST_HELPERS=\
	megabyte_writer kilobyte_writer\
	empty_out nl_out trail_nl_out start_nl_out interleave_err_out

test: build $(addprefix test_,$(addsuffix .native, $(TEST_HELPERS)))
	$(OCAMLBUILD) lib_test/test.native
	./test.native

install:
	ocamlfind install $(FINDLIB_NAME) META \
		$(INSTALL) \
		$(ARCHIVES)

uninstall:
	ocamlfind remove $(FINDLIB_NAME)

reinstall: uninstall install

clean:
	ocamlbuild -clean
