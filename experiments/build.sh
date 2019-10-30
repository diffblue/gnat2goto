#!/bin/sh

# Usage info
usage()
{
   if [ -n "$*" ]; then
      echo "Error: $*"
      echo
   fi

   echo "Usage:build.sh [--help] [--apex] [--adc pragmas.adc] path_to_ada_source_folder"
   echo
   echo "Run GNAT2Goto on an Ada repository."
   echo
   echo "The output is an ordered list of currently unsupported features"
   echo "with the number of times they occur in the input repository."
   echo
   echo "Options:"
   echo "  --help             Display this usage information"
   echo "  --apex             Use Rational APEX style naming convention .1.ada and .2.ada"
   echo "  --adc pragmas.adc  Use the specified 'pragmas.adc' file during compilation"
}

# Display a status message from this script
status()
{
   echo "[build.sh] $*"
}

# File extensions to expect for Specification files and Body files
spec_ext="${SPEC_EXT:-ads}"
body_ext="${BODY_EXT:-adb}"

# First check some environment prerequisites
status "Checking environment..."

# gnat2goto on the path?
if ! command -v gnat2goto > /dev/null; then
  status "gnat2goto not on PATH!"
  gnat2goto_bin=$(cd "$(dirname ${0})/../gnat2goto/install/bin" 2>/dev/null && pwd)
  status "Suggested adding gnat2goto to your PATH with the following command:"
  status "  export PATH=\"${gnat2goto_bin}:\${PATH}\""
  exit 1
fi

# gnat on the path?
if ! command -v gnat > /dev/null; then
   status "Gnat not on PATH!"
   # Take a wild guess at where gnat might be installed...
   gnat_location="/opt/gnat/bin"
   if [ -x "${gnat_location}/gnat" ] ; then
      # Check if that's gnat 2016...
      if gnat --version | grep -q 'GNAT GPL 2016' ; then
         status "Suggested adding gnat to your PATH with the following command:"
         status "  export PATH=\"${gnat_location}:\${PATH}\""
      fi
   fi
   exit 4
fi

# Right version of gnat?
if ! (gnat --version | grep -q 'GNAT GPL 2016') ; then
   status "The wrong version of gnat is on the PATH. gnat2goto requires GNAT GPL 2016."
   exit 5
fi

# Sane ADA_INCLUDE_PATH and GPR_PROJECT_PATH?
GNAT2GOTO=`command -v gnat2goto`
ADA_HOME=`command -v gnat`
ADA_HOME=$(cd "$(dirname ${ADA_HOME})/.." 2>/dev/null && pwd)
PLATFORM=`${ADA_HOME}/bin/gcc -dumpmachine`
DEF_ADA_INCLUDE_PATH="${ADA_HOME}/lib/gcc/${PLATFORM}"
ADA_GCC_VERSION=`${ADA_HOME}/bin/gcc -dumpversion`
DEF_ADA_INCLUDE_PATH="${DEF_ADA_INCLUDE_PATH}/${ADA_GCC_VERSION}/rts-native/adainclude:${ADA_HOME}/include"
export ADA_INCLUDE_PATH="${ADA_INCLUDE_PATH:-$DEF_ADA_INCLUDE_PATH}"

if [ -n "$GPR_PROJECT_PATH" -a ! -d "$GPR_PROJECT_PATH" ]; then
   status "GPR project path environment variable has been set to:"
   status "    \"${GPR_PROJECT_PATH}\""
   status "but that path is not a directory."
   status "Please set the environment variable GPR_PROJECT_PATH to the"
   status "location of the gnat libraries"
   exit 6
else
   # GPR_PROJECT_PATH not previously specified, use a default
   export GPR_PROJECT_PATH="${ADA_HOME}/lib/gnat"
   if [ ! -d "$GPR_PROJECT_PATH" ]; then
      status "Could not find gnat library directory at ${GPR_PROJECT_PATH}"
      status "Please set the environment variable GPR_PROJECT_PATH to the"
      status "location of the gnat libraries"
      exit 9
   fi
fi

status "...environment is OK."

# Command line processing....

if [ "$#" -eq 0 ]; then
   usage
   exit
fi

while [ -n "$1" ] ; do
   case "$1" in
      --apex)
         spec_ext="1.ada"
         body_ext="2.ada"
         pragma_file="$(dirname ${0})/rational-apex.adc"
         ;;
      --adc)
         shift
         if [ -r "${1}" ] ; then
            pragma_file="${1}"
         else
            usage "--adc option must specify a configuration pragma file"
            exit 2
         fi
         ;;
      --help)
         usage
         exit
         ;;
      *)
         if [ -n "$project_dir" ]; then
            usage "Only a single folder name may be specified"
            exit 2
         fi
         project_dir="$1"
   esac
   shift
done

if [ -z "$project_dir" ]; then
   usage >&2
   echo >&2 "A project folder name must be specified"
   exit 2
fi

# Finally start work
status "Project to build: ${project_dir}"

# Canonicalise the project_dir to an absolute path
project_root="$(cd "$(dirname "${project_dir}")"; pwd)/$(basename "${project_dir}")"
include_path=""

for foldername in $(find ${project_root} -type d -name "*" | LC_ALL=posix sort ); do
   count=`ls -1 ${foldername}/*.${spec_ext} 2>/dev/null | wc -l`
   if [ $count != 0 ]
   then
      include_path="${include_path} -I ${foldername}"
   fi
done

# Enumerate all the sub directories of ADA_INCLUDE_PATH
for include_folder in `echo "$ADA_INCLUDE_PATH" | tr ':' ' '` ; do
   status "Expanding $include_folder..."
   for foldername in $(find ${include_folder} -type d -name "*" | LC_ALL=posix sort); do
      count=`ls -1 ${foldername}/*.${spec_ext} 2>/dev/null | wc -l`
      if [ $count != 0 ]
      then
         ADA_INCLUDE_PATH="${ADA_INCLUDE_PATH}:${foldername}"
      fi
   done
done

if [ -n "${pragma_file}" ]; then
   pragma_file_opt="-gnatec=${pragma_file}"
fi

# Before attempting to start compiling, make a clear log of the environment
# we will be using:
status "-------------------------------------------------------"
status "gnat2goto =" "${GNAT2GOTO}"
status "ada gcc version =" "${ADA_GCC_VERSION}"
status "ADA_INCLUDE_PATH =" "${ADA_INCLUDE_PATH}"
status "GPR_PROJECT_PATH =" "${GPR_PROJECT_PATH}"
status "addtional include path flags:"
status "${include_path}"
status "-------------------------------------------------------"
# Log whether a file failed to compile due to incorrect syntax, or
# some other error that caused the compiler to exit with non-zero
# exit code
compile_error_occured=0
for filename in $(find ${project_root} -name "*.${body_ext}" | LC_ALL=posix sort); do
   status "Compiling ${filename}..."
   "${GNAT2GOTO}" ${pragma_file_opt} -gnatU ${include_path} "${filename}"
   result=$?

   if [ "$result" -gt 0 ]; then
      compile_error_occured=1
      status "  [COMPILE ERROR][EXIT CODE: $result]"
   else
      status "  [OK]"
   fi
done

if [ "$compile_error_occured" -gt 0 ]; then
   exit 7
fi
