# Compiling

## Build requirements (for Debian)

Note: you will *not* be able to build using the debian/ubuntu gnat and gprbuild.

Install the following debian packages:
  - build-essential

Download the following packages from http://libre.adacore.com:
  - GNAT GPL 2016 (`wget -qO- http://mirrors.cdn.adacore.com/art/5739cefdc7a447658e0b016b | tar xvz`)
  - GNATCOLL GPL 2016 (`wget -qO- http://mirrors.cdn.adacore.com/art/5739942ac7a447658d00e1e7 | tar xvz`)

Install GNAT GPL 2016 into /opt/gnat.

Install GNATCOLL GPL 2016:
  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ ./configure --prefix=/opt/gnat --disable-shared --enable-build=Debug`
  - `$ make` (its safe to use -jN here if you want)
  - `$ make install`

## Building gnat2goto

  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ export GPR_PROJECT_PATH=/opt/gnat/lib/gnat`
  - `$ make setup`
  - `$ make`

This should build into install/bin. You can test if it works by calling:

` $ install/bin/gnat2goto examples/foo.adb`
