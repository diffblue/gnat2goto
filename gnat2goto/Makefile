#
# gnat2goto
#
# This Makefile is heavily based on the SPARK 2014 makefile.
#


GPRBUILD=gprbuild

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

# symbolic linking of GNAT files is fast and that's how CodePeer is built too
USE_LN=true
uname:=$(shell uname)

# Building with GPL 2016
WORKAROUND_FILES=s-rident.ads

ifeq ($(filter-out CYGWIN%,$(uname)),)
  LDFLAGS=-Wl,--stack=0x2000000
  USE_LN=false
endif

.PHONY: setup force clean

all: build

setup:
	mkdir -p obj obj-tools install/bin
	sed -e "s^@ADAINCLUDE@^$(adainclude)^" \
	    -e "s^@ADALIB@^$(adalib)^" \
	    -e "s^@PREFIX@^$(prefix)^" \
	    -e "s^@GNAT1DIR@^$(gnat1dir)^" \
	    driver/sdefault.adb.in > obj/sdefault.adb
	for f in `cd gnat_src; ls xtreeprs.adb xnmake.adb xutil.ad? *-tmpl sinfo.ads treeprs.adt nmake.adt xsnamest.adb`; \
	do \
	  cp -p gnat_src/$$f obj-tools; \
	done
	cd obj-tools && gnatmake -q xtreeprs xnmake xsnamest && \
	./xtreeprs && ./xnmake && mv nmake.ads nmake.adb treeprs.ads ../obj && \
	./xsnamest && mv snames.ns ../obj/snames.ads && mv snames.nb ../obj/snames.adb
ifeq ($(USE_LN),true)
	cd obj && ln -sf ../gnat_src/*.ad[sb] .
else
	cd gnat_src && cp -p *.ads ../obj && cp -p *.adb ../obj
endif
	cd obj && $(RM) x*.ad? system* mlib* back_end.adb \
	  ada.ads gnat.ads interfac.ads unch*.ads text_io.ads && \
	$(RM) ?-*.ad? get_targ.adb gnatvsn.adb back_end.adb
ifeq ($(USE_LN),true)
	cd obj && \
	for f in $(WORKAROUND_FILES); \
	do \
	  ln -sf ../gnat_src/$$f .; \
	done
else
	cd gnat_src && cp -p $(WORKAROUND_FILES) ../obj
endif

build:
	$(GPRBUILD) $(GPRARGS) -Pgnat2goto.gpr -j$(PROCS) -cargs ${CFLAGS} -largs $(LDFLAGS)

clean:
	-$(RM) obj obj-tools
