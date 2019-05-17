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

def filter_gnat2goto_errout(errout_text):
    lines = errout_text.splitlines()
    result = ""
    for line in lines:
        if not line.startswith("Warning"):
            result += line
    return result

def process(debug, file, cbmcargs):
    """Process Ada file with gnat2goto and cbmc"""
    unit = os.path.splitext(file)[0]
    jsout    = unit + ".json_symtab"
    symtab   = unit + ".symtab"
    gotoprog = unit + ".goto_functions"
    out      = unit + ".out"
    errout   = unit + ".error"
    stdoutp  = unit + "stdoutp"
    cbmcerr  = unit + "cbmc_error"

    cmd = ["gnat2goto", file]

    g2go_results = Run(["gnat2goto", file], output=stdoutp, error=errout)

    stdout_file = open (stdoutp)
    errout_file = open (errout)
    stdout_text = stdout_file.read()
    errout_text = errout_file.read()
    stdout_file.close()
    errout_file.close()
    if stdout_text != '':
        print "Standard_Output from gnat2goto " + unit + ":"
        print stdout_text
    if filter_gnat2goto_errout(errout_text) != '':
        print "Standard_Error from gnat2goto " + unit + ":"
        print errout_text
    if g2go_results.status != 0:
        print "ERROR code ", g2go_results.status, " returned by gnat2goto when translating " + unit
        print "CBMC not run"
        return ""

    # only run the following if gnat2goto succeeded
    # Run(["cbmc", jsout, "--show-symbol-table"], output=symtab)
    # Run(["cbmc", jsout, "--show-goto-functions"], output=gotoprog)
    cmdline = ["cbmc", jsout]
    if cbmcargs: cmdline += cbmcargs.split(" ")
    results = Run(cmdline, error=cbmcerr)

    cbmcerr_file = open (cbmcerr)
    cbmcerr_text = cbmcerr_file.read()
    cbmcerr_file.close()
    if cbmcerr_text != '':
        print "Error from cbmc " + unit + ":"
        print cbmcerr_text
    if results.status != 0 and results.status != 10:
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

def prove(cbmcargs="", debug=False):
    """Call gnat2goto (and cbmc) on all *.adb files from the current directory

    PARAMETERS
      none: yet
    """
    for file in ada_body_files():
        out = process(debug, file, cbmcargs)
        if debug:
            print('<<< DEBUG ' + file + ' >>>')
            print(out)
            print('<<< END DEBUG ' + file + ' >>>')
        print(filter_cbmc_output(out))
