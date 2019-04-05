#!/bin/sh
if ! command -v gnat2goto > /dev/null; then
  echo >&2 "gnat2goto not on PATH!"
  exit 1
fi

if [ "$#" -ne 1 ]; then
   echo >&2 "Provide folder name to start"
   exit 2
fi

if [ "$1" = '--help' ]; then
   echo "Run GNAT2Goto on an Ada repository.\n"
   echo "The output is an ordered list of currently unsupported features"
   echo "with the number of times they occur in the input repository.\n"
   echo "The script builds a parsing program using collect_unsupported.cpp and expects"
   echo "this file to be in the same folder.\n"
   echo "Usage:\n\nlist_unsupported.sh path_to_ada_source_folder"
   exit 3
fi

echo >&2 "Project to build: $1"
path="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"
include_path=""
DIR=`dirname "$0"`
file_name=$(basename "$1")

for foldername in $(find ${path} -type d -name "*" | LC_ALL=posix sort ); do
   count=`ls -1 ${foldername}/*.ads 2>/dev/null | wc -l`
   if [ $count != 0 ]
   then
      include_path="${include_path} -I ${foldername}"
   fi
done

echo "$1: Unsupported features\n" > "$file_name".txt

if ! command -v gnat > /dev/null; then
   echo >&2 "Gnat not on PATH!"
   exit 4
fi

GNAT2GOTO=`command -v gnat2goto`
ADA_HOME=`command -v gnat`
ADA_HOME="$(dirname "$ADA_HOME")/.."
PLATFORM=`${ADA_HOME}/bin/gcc -dumpmachine`
DEF_ADA_INCLUDE_PATH="${ADA_HOME}/lib/gcc/${PLATFORM}"
ADA_GCC_VERSION=`${ADA_HOME}/bin/gcc -dumpversion`
DEF_ADA_INCLUDE_PATH="${DEF_ADA_INCLUDE_PATH}/${ADA_GCC_VERSION}/rts-native/adainclude:${ADA_HOME}/include"
export ADA_INCLUDE_PATH="${ADA_INCLUDE_PATH:-$DEF_ADA_INCLUDE_PATH}"

export GPR_PROJECT_PATH="${GPR_PROJECT_PATH:-/opt/gnat/lib/gnat}"
if [ ! -d "$GPR_PROJECT_PATH" ]; then
   echo >&2 "GPR project path does not exists!"
   exit 6
fi

# Enumerate all the sub directories of ADA_INCLUDE_PATH
for include_folder in `echo "$ADA_INCLUDE_PATH" | tr ':' ' '` ; do
   echo "Expanding $include_folder..."
   for foldername in $(find ${include_folder} -type d -name "*" | LC_ALL=posix sort); do
      count=`ls -1 ${foldername}/*.ads 2>/dev/null | wc -l`
      if [ $count != 0 ]
      then
         ADA_INCLUDE_PATH="${ADA_INCLUDE_PATH}:${foldername}"
      fi
   done
done

# Before attempting to start compiling, make a clear log of the environment
# we will be using:
echo >&2 "-------------------------------------------------------"
echo >&2 "gnat2goto =" "${GNAT2GOTO}"
echo >&2 "ada gcc version =" "${ADA_GCC_VERSION}"
echo >&2 "ADA_INCLUDE_PATH =" "${ADA_INCLUDE_PATH}"
echo >&2 "GPR_PROJECT_PATH =" "${GPR_PROJECT_PATH}"
echo >&2 "addtional include path flags:"
echo >&2 "${include_path}"
echo >&2 "-------------------------------------------------------"
# Log whether a file failed to compile due to incorrect syntax, or
# some other error that caused the compiler to exit with non-zero
# exit code
compile_error_occured=0
for filename in $(find ${path} -name '*.adb' | LC_ALL=posix sort); do
   printf "Compiling %s..." "${filename}" >&2
   echo "---------- COMPILING: $filename" >>"$file_name".txt
   "${GNAT2GOTO}" -gnatU ${include_path} "${filename}" > "$file_name".txt.compiling 2>&1
   result=$?
   cat "$file_name".txt.compiling >> "$file_name".txt

   # Check if we managed to get a list of missing features or not
   if grep -q -E '^----------At:' "$file_name".txt.compiling ; then
      printf " [UNSUPPORTED FEATURES]\n" >&2
      echo "---------- MISSING FEATURES ----------------------------" >>"$file_name".txt
   elif [ "$result" -gt 0 ]; then
      compile_error_occured=1
      printf " [COMPILE ERROR]\n" >&2
      # If the gnat compiler (gnat2goto in this case) fails with a standard
      # compiler error message, it's exit code will be '5' - and those messages
      # will be collated later. In the case where the exit code is different, we
      # really don't know a priori what has gone wrong, so log the exit code if
      # its one of those cases and collate the exit codes later.
      if [ "$result" != "5" ]; then
         echo "gnat2goto exit code: $result"  >>"$file_name".txt
      fi
      echo "---------- FAILED ----------------------------" >>"$file_name".txt
   else
      printf " [OK]\n" >&2
      echo "---------- OK --------------------------------" >>"$file_name".txt
   fi
done

# This redacting system is really pretty crude...
sed '/^\[/ d' < "$file_name".txt | \
   sed 's/"[^"][^"]*"/"REDACTED"/g' | \
      sed 's/[^ ][^ ]*\.adb/REDACTED.adb/g' | \
         sed 's/[^ ][^ ]*\.ads/REDACTED.ads/g' \
   > "$file_name"_redacted.txt

# Collate and summarise unsupported features
g++ --std=c++14 "$DIR"/collect_unsupported.cpp -o CollectUnsupported
LC_ALL=posix ./CollectUnsupported "$file_name".txt

# Collate and summarize compile errors from builds that did not generate
# unsupported features lists

# Find all the error messages, dropping the initial file name, column, and
# line number, and any trailing "at filename:line" info, then sort and
# collate them into descending counts of unique error messages.
#
# Finally, then output them in a form that looks similar to the output of the
# CollectUnsupported program, like this:
#
# --------------------------------------------------------------------------------
# Occurs: 3 times
# Redacted compiler error message:
# redacted error message
# Raw compiler error message:
# unredacted error message
# --------------------------------------------------------------------------------
#
sed -n 's/^.*:[0-9]*:[0-9]*: error: //p' "$file_name".txt | \
   sed 's/ at [^ ][^ ]*:[0-9][0-9]*$//' | \
      LC_ALL=posix sort | uniq -c | LC_ALL=posix sort -k1,1nr -k2 | \
         awk '/^ *[0-9]+ .*$/ { \
            count=$1; \
            raw=$0; sub(/^ *[0-9]+ /, "", raw); \
            redacted=raw; gsub(/"[^"]+"/, "\"REDACTED\"", redacted); \
            print "--------------------------------------------------------------------------------"; \
            print "Occurs:", count, "times"; \
            print "Redacted compiler error message:"; \
            print redacted; \
            print "Raw compiler error message:"; \
            print raw; \
            print "--------------------------------------------------------------------------------"; \
         }'

# For any other kind of failure, we will have logged the exit code. To summarize
# these cases, we convert the log output into a single line 'canonical form'
# (replace newlines with '<<<>>>>') then we can sort and uniq the list, before
# then turning the resulting list back into the same format as the report above.
# Yes, it's ugly...
awk  '/^---------- COMPILING: / { \
         buf = ""; count=0; \
      } \
      /^gnat2goto exit code: [0-9]*/ { \
         sub(/^.*---------- COMPILING: [^<]*<<<>>>/,"",buf); \
         print buf "<<<>>>" $0; \
      } \
      { \
         buf = buf "<<<>>>" $0 \
      }' "$file_name".txt \
   | LC_ALL=posix sort | uniq -c | LC_ALL=posix sort -k1,1nr -k2 | \
      awk '/^ *[0-9][0-9]* .*/ { \
         print "--------------------------------------------------------------------------------"; \
         print "Occurs:", $1, "times"; \
         sub(/^ *[0-9][0-9]* */,"",$0); \
         gsub(/<<<>>>/,"\n", $0); \
         print $0; \
         print "--------------------------------------------------------------------------------"; \
      }'

# Gather overall statistics
total_count=`grep -c -E '^---------- COMPILING: ' "$file_name".txt`
fail_count=`grep -c -E '^---------- FAILED ' "$file_name".txt`
non_compile_error_fail_count=`grep -c -E '^gnat2goto exit code: [0-9]*' "$file_name".txt`
missing_feature_count=`grep -c -E '^---------- MISSING FEATURES ' "$file_name".txt`
ok_count=`grep -c -E '^---------- OK ' "$file_name".txt`


echo >&2 "--------------------------------------------------------"
echo >&2 "${total_count} files processed during feature collection."
echo >&2 "${fail_count} files failed to compile."
echo >&2 "${non_compile_error_fail_count} miscellaneous compiler failures."
echo >&2 "${missing_feature_count} files used features unsupported by gnat2goto."
echo >&2 "${ok_count} compiled successfully."
echo >&2 "See \"${file_name}.txt\" for details."
echo >&2 "-------------------------------------------------------"

if [ "$compile_error_occured" -gt 0 ]; then
   exit 7
fi
