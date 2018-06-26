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
    return [f for f in os.listdir(os.curdir)\
            if os.path.isfile(f) and fnmatch.fnmatch (f, "*.adb")]

def filter_timing(results):
    """Remove timing data from results"""

    skip = re.compile(r'(^Runtime decision procedure: \d+\.\d+$)|(^size of program expression: \d+ steps$)', re.MULTILINE)

    return re.sub(skip, r'', results)


def process(file, cbmcargs):
    """Process Ada file with gnat2goto and cbmc"""
    unit = os.path.splitext(file)[0]
    jsout    = unit + ".json_symtab"
    symtab   = unit + ".symtab"
    gotoprog = unit + ".goto_functions"
    out      = unit + ".out"
    errout   = unit + ".error"

    cmd = ["gnat2goto", file]

    Run(["gnat2goto", file], output=jsout, error=errout)
    # ??? only run the following if gnat2goto succeeded
    Run(["cbmc", jsout, "--show-symbol-table"], output=symtab)
    Run(["cbmc", jsout, "--show-goto-functions"], output=gotoprog)
    cmdline = ["cbmc", jsout]
    if cbmcargs: cmdline += cbmcargs.split(" ")
    results = Run(cmdline)

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
        out = process(file, cbmcargs)
        if debug:
            print('<<< DEBUG ' + file + ' >>>')
            print(out)
            print('<<< END DEBUG ' + file + ' >>>')
        print(filter_cbmc_output(out))
