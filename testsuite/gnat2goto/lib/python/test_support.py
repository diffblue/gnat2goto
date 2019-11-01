"""
This module contains support functions for all test.py files
"""

import json
import fnmatch
import os
import re
import sys
from gnatpython.ex import Run

# Change directory
TEST = sys.modules['__main__']
TESTDIR = os.path.dirname(TEST.__file__)
TEST_NAME = os.path.basename(TESTDIR)
os.chdir(TESTDIR)

def ada_body_files():
    """Return list of *.adb files in the current folder."""
    files = os.listdir(os.curdir)
    files.sort()
    return [f for f in files\
            if os.path.isfile(f) and fnmatch.fnmatch (f, "*.adb")]

def filter_timing(results):
    """Remove timing data from results"""

    skip = re.compile(r'(^Runtime decision procedure: \d+\.\d+$)|(^size of program expression: \d+ steps$)', re.MULTILINE)

    return re.sub(skip, r'', results)

def process_gnat2goto(debug, file, is_main):
    """Process Ada file with gnat2goto and cbmc"""
    unit = os.path.splitext(file)[0]
    jsout    = unit + ".json_symtab"
    errout   = unit + ".error"
    stdoutp  = unit + "stdoutp"
    gnat2gotoargs = []
    if not is_main: gnat2gotoargs.append("--no-cprover-start")

    cmd = ["gnat2goto", file] + gnat2gotoargs

    g2go_results = Run(cmd, output=stdoutp, error=errout)

    stdout_file = open (stdoutp)
    errout_file = open (errout)
    stdout_text = stdout_file.read()
    errout_text = errout_file.read()
    stdout_file.close()
    errout_file.close()
    if stdout_text != '':
        print "Standard_Output from gnat2goto " + unit + ":"
        print stdout_text
    if errout_text != '':
        print "Standard_Error from gnat2goto " + unit + ":"
        print errout_text
    if g2go_results.status != 0:
        print "ERROR code ", g2go_results.status, " returned by gnat2goto when translating " + unit
        print "CBMC not run"
        return False

    return True

# only run the following if gnat2goto succeeded
def process_cbmc(debug, files, cbmcargs):
    """Process Ada file with gnat2goto and cbmc"""
    jsout = []
    for file in files:
        unit          = os.path.splitext(file)[0]
        file_jsout    = unit + ".json_symtab"
        jsout.append(file_jsout)

    cbmcerr = TEST_NAME + "cbmc_error"

    cmdline = ["cbmc"] + jsout
    if cbmcargs: cmdline += cbmcargs.split(" ")
    results = Run(cmdline, error=cbmcerr)

    cbmcerr_file = open (cbmcerr)
    cbmcerr_text = cbmcerr_file.read()
    cbmcerr_file.close()
    CBMC_VER_SUCC_CODE=0
    CBMC_VER_FAIL_CODE=10
    if cbmcerr_text != '':
        print "Error from cbmc " + unit + ":"
        print cbmcerr_text
    if results.status != CBMC_VER_SUCC_CODE and results.status != CBMC_VER_FAIL_CODE:
        print "ERROR code ", results.status, "returned by cbmc when processing " + unit

    return filter_timing(results.out)

def cbmc_match(line):
    return ('FAILURE' in line or
           'SUCCESS' in line or
           'SUCCEEDED' in line or
           'FAILED' in line)

def filter_cbmc_output(cbmc_output):
    lines = cbmc_output.split("\n")
    return "\n".join(filter(cbmc_match, lines))

def prove(cbmcargs="", main="", debug=False):
    """Call gnat2goto (and cbmc) on all *.adb files from the current directory

    PARAMETERS
      cbmcargs: arguments to be passed to CBMC
      main: name of the file containing the entry point
      debug: flag indicating if debug output should be printed out
    """
    for file in ada_body_files():
        gnat2goto_success = process_gnat2goto(debug, file, main == "" or file == main)
        if not gnat2goto_success:
            return ""
    out = process_cbmc(debug, ada_body_files(), cbmcargs)
    if debug:
        print('<<< DEBUG ' + file + ' >>>')
        print(out)
        print('<<< END DEBUG ' + file + ' >>>')
    print(filter_cbmc_output(out))
