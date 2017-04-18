Overview
========

This directory contains test and code for executing them. It is organized in
the same way as testsuites for other projects related to GNAT.

Individual testcases are located in subdirectories of tests/. They typically
contain:

* driver in `test.py` (in Python, or `test.cmd` in shell)
* expected outputs in `test.out`
* options in `test.opt`

Rationale and alternatives
--------------------------

Having the testsuite in GNAT format minimizes the mental effort related to
switching between different GNAT related products. Alternatively, we could
reuse the testsuite format of:

* cbmc, but it doesn't seem to support custom test, e.g. processing
  intermediate results in JSON format
* DejaGNU, but its design around Expect is widely criticized and it doesn't
  seem to fit our needs; also, it doesn't support parallel testing
* QMTest, but it seems dead now

See also:
* http://www.gnu.org/software/hurd/open_issues/unit_testing.html
* http://llvm.org/docs/CommandGuide/lit.html
* http://www.airs.com/blog/archives/499

Requirements
------------

```
$ apt-get install python-gnatpython
```

Running tests
-------------

```
$ ./testsuite.py -j 4
$ ./update-expected-outputs
$ git diff
```

where ```-j``` sets the number of tests to run in parallel (default is 1).

Adding tests
------------

* create a subdirectory in `tests/`
* create some .adb files
* create a `test.py` driver file
* run test with `testsuite.py` followed by `./update-expected-outputs` to strip
  the expected test output from noise (e.g. CBMC timing)
* check the test subdirectory into the version control

Disabling and skipping tests
----------------------------

Tests that are broken, e.g. because of a recent change in the GNAT front end
that results in an unexpected AST or because it is not fixed yet, should be
disabled by a `test.opt` file with:

```
ALL XFAIL reason for disabling this test
```

Alternatively, tests can be skipped, e.g. because they trigger an infinite
loop, by a `test.opt` file with:

```
ALL SKIP reason for skipping this test
```

Tests can be selectively executed on discriminants, e.g. tests that are known
to take long time to execute might be have their `test.opt` file like:

```
!large SKIP only required to run before a release
```

or because they are known to cause problems on some platforms:

```
Darwin SKIP test too slow
```

Finally, test options from several lines are combined, e.g.

```
!large SKIP only required to run before a release
Darwin SKIP test too slow
```

### Unexpected OKs

If a test crashes or fails, but its expected output is known, it is still wise
to have its `test.out` in the repository. If such a test is incidentally fixed,
e.g. by a change in the front end, it will be reported as `UOK`, or "unexpected
OK".

### Known but wrong outputs

Also, if a test doesn't crash, but gives a wrong output, e.g. it demonstrates
that some run-time checks are missing, it is still wise to have its `test.out`
in the repository. It increases the coverage and might highlight problems that
otherwise wouldn't be noticed. However, this is not a rule: in doubt use common
sense.

Test support code
-----------------

Following the conventions of similar testsuites, the test support code is split
into:

* `testsuite.py`, for driving the entire testsuite
* `run-test`, for driving a single test
* `lib/python/test_support.py`, for routines that customize individual tests
