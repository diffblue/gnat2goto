CBMC?=~/gnat/src/cbmc
SYMLINKS=table.ads table.adb types.ads types.adb alloc.ads stringt.ads stringt.adb namet.ads namet.adb debug.ads debug.adb output.ads output.adb tree_io.ads tree_io.adb opt.ads opt.adb hostparm.ads widechar.ads widechar.adb gnatvsn.ads gnatvsn.adb
CVC4:=$(shell which cvc4)

ifdef CVC4
	GEN_FLAGS:=-O
else
	GEN_FLAGS:=
endif

all: unit_test

gnatvsn.adb: ../driver/gnatvsn.adb
	ln -s ../driver/$@

%.ads:
	ln -s ../gnat_src/$@

%.adb:
	ln -s ../gnat_src/$@

ireps.ads: ireps_generator.py
	./ireps_generator.py $(GEN_FLAGS) $(CBMC)

unit_test: unit_test.adb $(SYMLINKS) ireps.ads ireps-*.adb
	gprbuild -P unit_test.gpr -j0 -gnatg -g -gnata

clean:
	find . -type l -delete
	rm -f *.o *.ali
	rm -f *.stderr *.stdout
	rm -f b__*.ad[bs]
	rm -f unit_test unit_test.bexch
	rm -f tree.*
	rm -rf ireps.ad[bs]
	rm -f *.smt2 *.out
