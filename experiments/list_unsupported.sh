#!/bin/sh

# Usage info
usage()
{
   echo "Usage:list_unsupported.sh [--help] [--apex] [--adc pragmas.adc] path_to_ada_source_folder"
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

# File extensions to expect for Specification files and Body files
spec_ext="${SPEC_EXT:-ads}"
body_ext="${BODY_EXT:-adb}"

# Command line processing....

if [ "$#" -eq 0 ]; then
   usage
   exit
fi


build_opts=""
report_opts=""

script_dir="$(dirname "$0")"

while [ -n "$1" ] ; do
   case "$1" in
      --apex)
         build_opts="${build_opts} --apex"
         report_opts="${report_opts} --apex"
         spec_ext="1.ada"
         body_ext="2.ada"
         ;;
      --adc)
         shift
         if [ -r "${1}" ] ; then
            build_opts="${build_opts} --adc ${1}"
         else
            usage >&2
            echo >&2 "--adc option must specify a configuration pragma file"
            exit 2
         fi
         ;;
      --help)
         usage
         exit
         ;;
      *)
         if [ -n "$project_dir" ]; then
            usage >&2
            echo >&2 "Only a single folder name may be specified"
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

echo "Project to build: ${project_dir}"
project_name=$(basename "${project_dir}")
canonical_project_path="$(cd "${project_dir}"; pwd)"
include_path=""

"${script_dir}/build.sh" ${build_opts} "${canonical_project_path}" > "${project_name}.stdout.txt" 2> "${project_name}.stderr.txt"
"${script_dir}/list_unsupported_report.sh" ${report_opts} "${project_name}.stdout.txt" "${project_name}.stderr.txt"

# Need to use ${spec_ext} and ${body_ext} inside some regex's here,
# so add any quoting necessary
quoted_spec_ext=$(printf "%s" "${spec_ext}" | sed 's/\./\\./g')
quoted_body_ext=$(printf "%s" "${body_ext}" | sed 's/\./\\./g')
# This redacting system is really pretty crude...
cat "${project_name}.stdout.txt" "${project_name}.stderr.txt" > "${project_name}.txt"
  sed '/^\[/ d' "${project_name}.txt" | \
    sed 's/"[^"][^"]*"/"REDACTED"/g' | \
      sed "s/[^ ][^ ]*\\.${quoted_body_ext}/REDACTED.${body_ext}/g" | \
        sed "s/[^ ][^ ]*\\.${quoted_spec_ext}/REDACTED.${spec_ext}/g" | \
          sed 's/at [-\_\.a-zA-Z0-9][-\_\.a-zA-Z0-9]*:[0-9][0-9]*/at REDACTED/g' | \
            sed 's/[-\_\.a-zA-Z0-9][-\_\.a-zA-Z0-9]*:[0-9][0-9]*:[0-9][0-9]*/REDACTED/g' \
   > "${project_name}_redacted.txt"

