#!/bin/sh

# Usage info
usage()
{
   if [ -n "$*" ]; then
      echo "Error: $*"
      echo
   fi

   echo "Usage:list_unsupported_report.sh [--help] [--apex] stdout_file stderr_file"
   echo
   echo "Analyse the results from a GNAT2Goto build."
   echo
   echo "The output is an ordered list of currently unsupported features"
   echo "with the number of times they occur in the input repository."
   echo
   echo "Options:"
   echo "  --help             Display this usage information"
   echo "  --apex             Use Rational APEX style naming convention .1.ada and .2.ada"
}

# Display a status message from this script
status()
{
   echo "[list_unsupported_report.sh] $*"
}

# File extensions to expect for Specification files and Body files
spec_ext="${SPEC_EXT:-ads}"
body_ext="${BODY_EXT:-adb}"

# First check some environment prerequisites
status "Checking environment..."
# Can we build the support tool?
# Problem: GNAT 2016 helpfully installs a g++ binary alongside gnat... except
# it's way out of date so invoking g++ via PATH looking will pickup that gnat g++
# compiler, rather than the default system compiler... Instead we need to
# temporarily drop the GNAT tools off the path while we build the tool.
ADA_HOME=`command -v gnat`
ADA_HOME=$(cd "$(dirname ${ADA_HOME})/.." 2>/dev/null && pwd)
export PATH=$(echo ${PATH} | tr ':' '\n' | grep -v "${ADA_HOME}" | paste -s -d : - )
experiment_dir=`dirname "$0"`
gplusplus=`command -v g++`
if ! ${gplusplus} --std=c++14 "${experiment_dir}/collect_unsupported.cpp" -o CollectUnsupported ; then
   status "Failed to compile support tool 'CollectUnsupported' using ${gplusplus}"
   status "You need a version of g++ on the PATH that supports C++14"
   exit 8
fi

status "...environment is OK."


# Command line processing....

if [ "$#" -eq 0 ]; then
   usage
   exit
fi

input_stdout=""
input_stderr=""

while [ -n "$1" ] ; do
   case "$1" in
      --apex)
         spec_ext="1.ada"
         body_ext="2.ada"
         pragma_file="$(dirname ${0})/rational-apex.adc"
         ;;
      --help)
         usage
         exit
         ;;
      --*)
         usage
         exit
         ;;
      *)
         if [ -z "${input_stdout}" ]; then
            input_stdout="$1"
         elif [ -z "${input_stderr}" ]; then
            input_stderr="$1"
         else
            usage "Only two input files may be specified"
            exit
         fi
         ;;
   esac
   shift
done

# Finally start work

# Collate the two input files for ease of processing
raw_input_file="list_unsupported_report.tmplog"
cat "${input_stdout}" "${input_stderr}" > "${raw_input_file}"

# Need to use ${spec_ext} and ${body_ext} inside some regex's here,
# so add any quoting necessary
quoted_spec_ext=$(printf "%s" "${spec_ext}" | sed 's/\./\\./g')
quoted_body_ext=$(printf "%s" "${body_ext}" | sed 's/\./\\./g')

# Collate and summarise unsupported features
LC_ALL=posix ./CollectUnsupported "$raw_input_file"

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
sed -n 's/^.*:[0-9]*:[0-9]*: error: //p' "$raw_input_file" | \
   sed 's/ at [^ ][^ ]*:[0-9][0-9]*$//' | \
      LC_ALL=posix sort | uniq -c | LC_ALL=posix sort -k1,1nr -k2 | \
         awk '/^ *[0-9]+ .*$/ { \
            count=$1; \
            raw=$0; sub(/^ *[0-9]+ /, "", raw); \
            redacted=raw; \
            gsub(/"[^"]+"/, "\"REDACTED\"", redacted); \
            gsub(/at ([a-zA-Z0-9\.\-_]+:[0-9]+)/, "at REDACTED", redacted); \
            print "--------------------------------------------------------------------------------"; \
            print "Occurs:", count, "times"; \
            print "Redacted compiler error message:"; \
            print redacted; \
            print "Raw compiler error message:"; \
            print raw; \
            print "--------------------------------------------------------------------------------"; \
         }'

# For GNAT BUG DETECTED
awk  '/^\+===========================GNAT BUG DETECTED==============================\+/ { \
         buf = $0; \
      } \
      /^\+==========================================================================\+/ { \
         print buf "<<<>>>" $0; buf = ""\
      } \
      /^\| Error detected at .*$/ { \
         buf = buf "<<<>>>" "Error detected at REDACTED" \
      } \
      /^\| .*$/ { \
         buf = buf "<<<>>>" $0 \
      } \
      /^[^|\+].*/ { \
         if (length(buf) > 0) { \
            print buf "<<<>>>" $0; buf = "" \
         } \
      }' "$raw_input_file" \
   | LC_ALL=posix sort | uniq -c | LC_ALL=posix sort -k1,1nr -k2 | \
      awk '/^ *[0-9][0-9]* .*/ { \
         print "--------------------------------------------------------------------------------"; \
         print "Occurs:", $1, "times"; \
         sub(/^ *[0-9][0-9]* */,"",$0); \
         gsub(/<<<>>>/,"\n", $0); \
         print $0; \
         print "--------------------------------------------------------------------------------"; \
      }'

# For exceptions - note that we don't redact the file name here, because the filename/location
# will be one of our own gnat2goto or gnat source files, not the users code.
awk  '/^raised .*$/ { \
         if (prev == "<========================>") \
            print prev "<<<>>>" $0 "<<<>>>" ; \
      } \
      { \
         prev = $0 \
      }' "$raw_input_file" \
   | LC_ALL=posix sort | uniq -c | LC_ALL=posix sort -k1,1nr -k2 | \
      awk '/^ *[0-9][0-9]* .*/ { \
         print "--------------------------------------------------------------------------------"; \
         print "Occurs:", $1, "times"; \
         sub(/^ *[0-9][0-9]* */,"",$0); \
         gsub(/<<<>>>/,"\n", $0); \
         print $0; \
         print "--------------------------------------------------------------------------------"; \
      }'
