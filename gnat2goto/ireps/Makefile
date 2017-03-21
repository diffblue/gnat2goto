CBMC?=~/gnat/src/cbmc
SYMLINKS=table.ads table.adb types.ads types.adb alloc.ads stringt.ads stringt.adb namet.ads namet.adb debug.ads debug.adb output.ads output.adb tree_io.ads tree_io.adb opt.ads opt.adb hostparm.ads widechar.ads widechar.adb gnatvsn.ads gnatvsn.adb

all: unit_test

gnatvsn.adb: ../driver/gnatvsn.adb
	ln -s ../driver/$@

%.ads:
	ln -s ../gnat_src/$@

%.adb:
	ln -s ../gnat_src/$@

ireps.ads: ireps_generator.py
	./ireps_generator.py $(CBMC)

unit_test: unit_test.adb $(SYMLINKS) ireps.ads ireps-*.adb
	gnatmake -gnatg -g -gnata unit_test.adb

clean:
	find . -type l -delete
	rm -f *.o *.ali
	rm -f b~*.ad[bs]
	rm -f unit_test
	rm -f tree.*
	rm -rf ireps.ad[bs]
