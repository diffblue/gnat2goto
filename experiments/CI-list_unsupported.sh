#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")"; pwd)"
CI_WORKING_DIR="${SCRIPT_DIR}/ada-ci-projects"
CI_GOLDEN_RESULTS="${SCRIPT_DIR}/golden-results"


# Accumulator variable to gather a list of projects
# that failed
ci_failed_projects=""

test_all()
{
	ci_failed=0

	while read repo hash
	do
		test_repo ${repo} ${hash} || ci_failed=1
	done <<-REPOS
		https://github.com/tum-ei-rcs/StratoX.git 5fdd04e01a25efef6052376f43ce85b5bc973392
		https://github.com/martin-cs/Tokeneer.git e7cbd20d31eeddd86e47e96cfcf748b060e8eead
		https://github.com/martin-cs/UKNI-Information-Barrier.git ebef4042b25118e64d3b53f6b570c980c8eb57ee
		https://github.com/damaki/ksum.git 31c0104085ccd818b219bd66d048a6d406b1ba3a
		https://github.com/damaki/libkeccak.git c2ab9b9e83d0383b13212ebbfd375a5b9c1a993f
		https://github.com/Componolit/libsparkcrypto.git adde5fc6cef607a890fb279bb3fd80fe761d3225
		https://git.codelabs.ch/muen.git 41263af9482b85799537d4cad3d79427f153d7d3
		https://github.com/martin-cs/SPARK-tetris.git c7dce756f9ec4166afe3033a094f65bb81254fc3
	REPOS

	if [ "$(uname)" = "Linux" ]; then
		# VCT currently gives different compiler errors on OSX and Linux due to differences
		# in the implementation of the GNAT system.ads file. We therefore only run this project
		# on Linux until such times as we can handle that file.
		test_repo 'https://github.com/pauljaxon/vct.git' '18439b3337eb36e896fa89507c102d919692fcec' || ci_failed=1
	fi

	if [ "$ci_failed" -gt 0 ]; then
		echo_bold_red "FAILED CI-list_unsupported.sh: Golden results out of date for the following projects:"
		for project in ${ci_failed_projects}; do
			echo_red "    $project"
		done
	else
		echo_bold_green "PASSED CI-list_unsupported.sh: Golden results up to date."
	fi

	exit $ci_failed
}

test_repo()
{
	repo=$1
	commit=$2
	repo_checkout_dir="$(basename ${repo%.git})"

	echo "Testing list_unsupported.sh against ${repo} commit ${hash} ..."

	if [ ! -d "${CI_WORKING_DIR}" ] ; then
		mkdir "${CI_WORKING_DIR}"
	fi

	(
		cd "${CI_WORKING_DIR}"

		if [ ! -d "${repo_checkout_dir}" ]; then
			echo "Cloning ${repo} into ${repo_checkout_dir}..."
			git clone "${repo}"
		fi

		(
			cd "${repo_checkout_dir}"
			if [ "$(git rev-parse HEAD)" != "${commit}" ] ; then
				echo "Checking out ${hash} in ${repo_checkout_dir}..."
				git checkout --detach "${hash}"
			fi
		)

		echo "Gathering unsupported features..."
		"${SCRIPT_DIR}/build.sh" "${CI_WORKING_DIR}/${repo_checkout_dir}" > "${CI_WORKING_DIR}/${repo_checkout_dir}-stdout" 2> "${CI_WORKING_DIR}/${repo_checkout_dir}-stderr"
		"${SCRIPT_DIR}/list_unsupported_report.sh" "${CI_WORKING_DIR}/${repo_checkout_dir}-stdout" "${CI_WORKING_DIR}/${repo_checkout_dir}-stderr"  | \
			grep -A 3 'Occurs:' > "${CI_WORKING_DIR}/${repo_checkout_dir}-summary.txt"
	)

	echo "Checking against golden results..."
	diff -u "${CI_GOLDEN_RESULTS}/${repo_checkout_dir}-summary.txt" \
			"${CI_WORKING_DIR}/${repo_checkout_dir}-summary.txt"  \
		> "${CI_WORKING_DIR}/${repo_checkout_dir}-ci_result.txt"
	diff_result="$?"

	if [ "$diff_result" -gt 0 ]; then
		ci_failed_projects="${ci_failed_projects} ${repo_checkout_dir}"
		echo_red "FAILED: Golden results out of date for ${repo_checkout_dir}. Diff:"
		cat "${CI_WORKING_DIR}/${repo_checkout_dir}-ci_result.txt"
	else
		echo_green "PASSED: Golden results up to date for ${repo_checkout_dir}"
	fi

	return $diff_result
}

echo_red()
{
	red=$(tput setaf 1)
	normal=$(tput sgr0)
	echo "${red}$*${normal}"
}

echo_green()
{
	green=$(tput setaf 2)
	normal=$(tput sgr0)
	echo "${green}$*${normal}"
}

echo_bold_red()
{
	bold=$(tput bold)
	echo_red "${bold}$*"
}

echo_bold_green()
{
	bold=$(tput bold)
	echo_green "${bold}$*"
}

test_all
