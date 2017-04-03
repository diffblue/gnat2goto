#
# gnat2goto
#
# This Makefile is losely based on the SPARK 2014 makefile.
#

GPRBUILD=gprbuild
CBMC_SOURCE?=~/gnat/src/cbmc

# number of processors
PROCS=0

RM=rm -rf
MV=mv -f
# adainclude and adalib are later used as input for sed, so we replace
# backward slashes by forward slashes to avoid sed interpreting those
adainclude:=$(subst \,/,$(strip $(shell gnatls -v | grep adainclude)))
adalib:=$(subst \,/,$(strip $(shell gnatls -v | grep adalib)))
gnat1dir:=$(shell echo $(adainclude) | sed -e 's^rts-.*^^' -e 's^adainclude^^')

prefix:=@NOPREFIX@

ALL_FRONTEND_FILES:=$(notdir $(wildcard gnat_src/*.ad[bs]))

# Files part of the standard library. We don't want to copy these.
STDLIB_FILES:=$(notdir $(wildcard gnat_src/?-*.ad?)) ada.ads gnat.ads interfac.ads text_io.ads unchconv.ads unchdeal.ads $(notdir $(wildcard gnat_src/system*.ad?)) $(notdir $(wildcard gnat_src/mlib*.ad?))

# Code generator files we will deal with separately. We don't copy these.
FE_CODEGEN_FILES:=$(notdir $(wildcard gnat_src/x*.ad?))

# Files part of the compiler we wish to substitute with our own. Again, we
# don't copy these.
FE_REPLACED_FILES:=get_targ.adb gnatvsn.adb back_end.adb

# Building with GPL 2016 requires these files from the standard library. We
# normally would not copy them.
WORKAROUND_FILES=s-rident.ads

FE_FILES_TO_COPY:=$(filter-out $(STDLIB_FILES) $(FE_REPLACED_FILES) $(FE_CODEGEN_FILES), $(ALL_FRONTEND_FILES)) $(WORKAROUND_FILES)

SINFO_CODEGEN:=xtreeprs.adb xnmake.adb xsnamest.adb xutil.ads xutil.adb sinfo.ads treeprs.adt nmake.adt $(notdir $(wildcard gnat_src/*-tmpl))

GENERATED_FILES:=$(addprefix obj/, treeprs.ads nmake.ads nmake.adb snames.ads snames.adb sdefault.adb)

.PHONY: clean

all: build

obj:
	@mkdir obj

obj-tools:
	@mkdir obj-tools

obj-tools/%: gnat_src/% | obj-tools
	cp -p $< $@

obj-tools/xtreeprs: $(addprefix obj-tools/, $(SINFO_CODEGEN))
	cd obj-tools && gnatmake -q $(notdir $@)

obj-tools/xnmake: $(addprefix obj-tools/, $(SINFO_CODEGEN))
	cd obj-tools && gnatmake -q $(notdir $@)

obj-tools/xsnamest: $(addprefix obj-tools/, $(SINFO_CODEGEN))
	cd obj-tools && gnatmake -q $(notdir $@)

obj/treeprs.ads: obj-tools/xtreeprs | obj
	cd obj-tools && ./xtreeprs && cp treeprs.ads ../obj

obj/nmake%ads obj/nmake%adb: obj-tools/xnmake | obj
	cd obj-tools && ./xnmake && cp nmake.ad[bs] ../obj

obj/snames%ads obj/snames%adb: obj-tools/xsnamest | obj
	cd obj-tools && ./xsnamest && cp snames.ns ../obj/snames.ads && cp snames.nb ../obj/snames.adb

obj/sdefault.adb: driver/sdefault.adb.in | obj
	sed -e "s^@ADAINCLUDE@^$(adainclude)^" \
	    -e "s^@ADALIB@^$(adalib)^" \
	    -e "s^@PREFIX@^$(prefix)^" \
	    -e "s^@GNAT1DIR@^$(gnat1dir)^" \
	    driver/sdefault.adb.in > obj/sdefault.adb

obj/%: gnat_src/% | obj
	cp -p $< $@

ireps/ireps.ads: ireps/ireps_generator.py
	make -C ireps ireps.ads CBMC=$(CBMC_SOURCE)

build: $(GENERATED_FILES) $(addprefix obj/, $(FE_FILES_TO_COPY)) ireps/ireps.ads
	@mkdir -p install/bin
	$(GPRBUILD) $(GPRARGS) -Pgnat2goto.gpr -j$(PROCS) -cargs ${CFLAGS} -largs $(LDFLAGS)

clean:
	-$(RM) obj obj-tools
	make -C ireps clean
