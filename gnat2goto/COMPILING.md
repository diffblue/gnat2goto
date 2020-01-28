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


Install the GNAT GPL Ada 2016 using the `doinstall` script inside the zip. The following instructions assume you picked `/opt/gnat` as the install destination.

Install GNATCOLL GPL 2016:
  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ ./configure --prefix=/opt/gnat --disable-shared --enable-build=Debug --without-postgresql`
  - `$ make`
  - `$ make install`


# Building gnat2goto

Assuming that you've built the dependencies as per the above instructions, do the following from within `gnat2goto`.

  - `$ export PATH=/opt/gnat/bin:${PATH}`
  - `$ export GPR_PROJECT_PATH=/opt/gnat/lib/gnat`
  - `$ make`

This should build into install/bin. You can test if it works by calling:

` $ install/bin/gnat2goto examples/foo.adb`

If it has worked, you should have two files: `foo.ali` and `foo.json_symtab`.

# Building and running gnat2goto unit tests

gnat2goto includes a selection of unit tests. These are built as a seperate
`unit_tests` binary. Assuming you have built gnat2goto as per the above
instructions, the `unit_tests` binary can be built with the following command

`$ gprbuild -P unit_tests.gpr -cargs -largs`

This should build into install/bin. You can then run the unit tests by calling:

` $ install/bin/unit_tests`

# Running gnat2goto regression tests

In addition to the unit tests, gnat2goto also includes end-to-end regression
tests. These tests aim to test the full pipeline from Ada source code input
through to final analysis by CBMC.

Instructions for running the regression test suite are described in [testsuite/gnat2goto/README.md](../testsuite/gnat2goto/README.md)

# Running and updating missing features tests

Currently gnat2goto is lacking support for a number of Ada language features.
The CI system runs gnat2goto on a number of open source Ada projects to track
changes in feature coverage. These tests can be run locally by performing the
following commands:

- `$ export PATH=/opt/gnat/bin:/path/to/gnat2goto/bin:${PATH}`
- `$ export GPR_PROJECT_PATH=/opt/gnat/lib/gnat`
- `$ cd gnat2goto/experiments`
- `$ ./CI-list_unsupported.sh`

This script will checkout a number of Ada projects, then for each project it will
run gnat2goto and summarize the unsupported features used by the project. The
summary is then compared against a 'golden' reference summary.

If you make changes to gnat2goto that cause it to support previously unsupported
features you will need to then also update these 'golden' reference files. These
files are stored in `gnat2goto/experiments/golden-results/<project>-summary.txt`
one file per project. Simply copy the '<project>-summary.txt' file from
`gnat2goto/experiments/ada-ci-projects/<project>-summary.txt` into
`gnat2goto/experiments/golden-results/` and include that as part of your PR.

# Updating the CBMC submodule

As described in [testsuite/gnat2goto/README.md](../testsuite/gnat2goto/README.md)
gnat2goto includes a GIT submodule for CBMC. In most cases this should not need
changing, but if you are developing gnat2goto features or fixes that require a
different version of CBMC then you will need to update the CBMC submodule
pointer. This can be done by:

 - Pull in the upsteam CBMC version, rebasing so that any downstream changes
   to CBMC are maintained as a set of patches on top of a 'clean' CBMC.
   ```
   $ git submodule update --remote --rebase
   ```
 - Fix any merge conflicts caused by the rebase (at the time of writing, no
   downstream modfications have been made, so no merge conflicts should occur).
 - Rebuild the submodule CBMC, following the usual instructions in `lib/cbmc/COMPILING.md`
 - Run the regression tests as described above.
 - Fix any failing regression tests caused by the submodule update
 - Produce a PR containing both the submodule pointer change, and all the fixes
   required in gnat2goto to maintain the regression test results
