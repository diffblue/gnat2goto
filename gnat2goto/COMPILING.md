# Build Dependencies

**To build GnatColl GPL 2016 and gnat2goto you must use a gnat GPL 2016 compiler and gprbuild (see the GnatColl user documentation)**

We need the 2016 versions of the GNAT Ada compiler, the gprbuild system and the
GNATColl utility library to build GNAT2GOTO.

Note: you will *not* be able to build using the debian/ubuntu gnat and gprbuild, it *has* to be the 2016 release from AdaCore.

## OS specific dependencies

For **Debian**, install the following debian packages:
  - build-essential

For **Mac OSX**, you will need the XCode Command Line tools, which require XCode itself and then a 
```
xcode-select --install
```
to make them available.

----------------------------

Go to [GNAT Community Download page at AdaCore](https://www.adacore.com/download/more),
select your platform and then 2016 (in that order). Download

  * GNAT GPL Ada
  * gnatcoll-gpl-2016


The following assumes you installed GNAT GPL 2016 into /opt/gnat using the doinstall script.

Install GNATCOLL GPL 2016:
  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ ./configure --prefix=/opt/gnat --disable-shared --enable-build=Debug --without-postgresql`
  - `$ make`
  - `$ make install`


# Building gnat2goto

Assuming that you've built the dependencies as per the above instructions

  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ export GPR_PROJECT_PATH=/opt/gnat/lib/gnat`
  - `$ make`

This should build into install/bin. You can test if it works by calling:

` $ install/bin/gnat2goto examples/foo.adb`
