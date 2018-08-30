# To build GnatColl GPL 2016 and gnat2goto you must use a gnat GPL
# 2016 compiler (see the GnatColl user documentation)

# Compiling (Debian) See below for Mac OSX

## Build requirements (for Debian)

Note: you will *not* be able to build using the debian/ubuntu gnat and gprbuild.

Install the following debian packages:
  - build-essential

Download the following packages from http://libre.adacore.com:
  - GNAT GPL 2016 (`wget -qO- http://mirrors.cdn.adacore.com/art/5739cefdc7a447658e0b016b | tar xvz`)
  - GNATCOLL GPL 2016 (`wget -qO- http://mirrors.cdn.adacore.com/art/5a15cb87c7a4479a23674d44 | tar xvz`)

Install GNAT GPL 2016 into /opt/gnat using the doinstall script.

Install GNATCOLL GPL 2016:
  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ ./configure --prefix=/opt/gnat --disable-shared --enable-build=Debug`
  - `$ make` (its safe to use -jN here if you want)
  - `$ make install`

## Building gnat2goto

  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ export GPR_PROJECT_PATH=/opt/gnat/lib/gnat`
  - `$ make`

This should build into install/bin. You can test if it works by calling:

` $ install/bin/gnat2goto examples/foo.adb`

# Compiling Mac OSX

## Build requirements (for Mac OSX Sierra and later)
1 Install or upgrade Xcode from App-store and then in a terminal widow
type:
  ```
   xcode-select --install
   ```
2 Download the following packages from http://libre.adacore.com:
   From the download page click on "More packages, platforms, versions
   and sources »"
   Select "x86-64 Mac OS X (64bits)" in the platform selection box and "2016" in
   the year selection box.
   Click on "gnat-gpl-2016-x86_64-darwin-bin.tar.gz" to download gnat
   Click on "gnatcoll-gpl-2016-src-m1.tar.gz" to download GnatColl

3 From a Finder window click on each of the downloaded files to expand
them

4 Install gnat GPL 2016: In a terminal window cd to the directory
containing the expanded gnat-gpl-2016-x86_64-darwin-bin and execute
sudo ./doinstall and install into /opt/gnat

5 Install GnatColl GPL 2016: In a terminal window cd to the directory
containing the expanded gnatcoll-gpl-2016-src-m1 and type
 - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ ./configure --prefix=/opt/gnat --disable-shared --enable-build=Debug --without-postgresql`
  - `$ make` (its safe to use -jN here if you want)
  - `$ sudo make install`

## Building gnat2goto
1 cd to the expanded gnat2goto directory and then to the next level
gnat2goto directory and type
  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ export GPR_PROJECT_PATH=/opt/gnat/lib/gnat`
  - `$ make`

2 This should build into install/bin. You can test if it works by calling:

` $ install/bin/gnat2goto examples/foo.adb`







