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


def process(file):
    """Process Ada file with gnat2goto and cbmc"""
    unit = os.path.splitext(file)[0]

    jsout    = unit + ".json_symtab"
    symtab   = unit + ".symtab"
    gotoprog = unit + ".goto_functions"
    out      = unit + ".out"

    cmd = ["gnat2goto", file]

    Run(["gnat2goto", file], output=jsout)
    # ??? only run the following if gnat2goto succeeded
    Run(["cbmc", jsout, "--show-symbol-table"], output=symtab)
    Run(["cbmc", jsout, "--show-goto-functions"], output=gotoprog)
    results = Run(["cbmc", jsout])

    return filter_timing(results.out)


def prove():
    """Call gnat2goto (and cbmc) on all *.adb files from the current directory

    PARAMETERS
      none: yet
    """
    for file in ada_body_files():
        print process (file)
