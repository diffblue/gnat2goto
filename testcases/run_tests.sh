#!/bin/bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
G2G=$DIR/../gnat2goto/install/bin/gnat2goto
CBMC=$DIR/cbmc # Make this link if it doesn't already exist

if [ ! -f $G2G ]; then
   echo "$G2G not found (run gprbuild in gnat2goto/gnat2goto?)"
   exit 1;
fi

if [ ! -f $CBMC ]; then
   echo "$CBMC not found (make a symlink 'cbmc' in the testcases directory)"
   exit 1;
fi

for prog in $DIR/*.adb; do
   echo "Running: $prog"
   JSOUT=${prog/adb/json_symtab}
   SYMTAB=${prog/adb/symtab}
   GOTOPROG=${prog/adb/goto_functions}
   OUTPUT=${prog/adb/out}
   $G2G $prog > $JSOUT
   $CBMC $JSOUT --show-symbol-table > $SYMTAB
   $CBMC $JSOUT --show-goto-functions > $GOTOPROG
   $CBMC $JSOUT > $OUTOUT 2>&1
done

echo "All tests passed!"
