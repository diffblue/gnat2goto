#!/usr/bin/env python
"""Run the testsuite for echo

This module assumes that lib/python has been added to PYTHONPATH.
"""

from gnatpython.env import Env
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 generate_collect_result,
                                 generate_run_testcase,
                                 setup_result_dir)
from gnatpython.testdriver import add_run_test_options
from gnatpython.reports import ReportDiff
from gnatpython.fileutils import (split_file)
from glob import glob
import os
import sys


def main():
    """Run the testsuite"""

    m = Main()
    add_mainloop_options(m, extended_options=True)
    add_run_test_options(m)
    m.add_option("--diffs", dest="view_diffs", action="store_true",
                 default=False, help="show diffs on stdout")
    m.parse_args()

    # Various files needed or created by the testsuite
    # creates :
    #   the ouput directory (out by default)
    #   the report file
    #   the results file

    setup_result_dir(m.options)

    if m.args:
        test_list = [t.strip('/') for t in m.args]
    else:
        test_list = sorted(glob('tests/*'))

    env = Env()

    # add support module path
    python_lib = os.path.join(os.getcwd(), 'lib', 'python')
    Env().add_search_path("PYTHONPATH", python_lib)

    env.add_search_path('PYTHONPATH', os.getcwd())
    discs = [env.target.platform]

    if m.options.discs:
        discs += m.options.discs.split(',')

    test_metrics = {'total': len(test_list), 'uok': 0}

    # Generate a standard 'collect_result' function...
    generated_collect_result = generate_collect_result(
        result_dir = m.options.output_dir,
        results_file = m.options.results_file,
        output_diff = m.options.view_diffs,
        metrics = test_metrics)

    # ... and then wrap that generated 'collect_result' function in something
    # that will also accumulate 'UOK' test results
    def collect_result_including_uok(name, process, _job_info):
        generated_collect_result(name, process, _job_info)
        test_name = os.path.basename(name)
        test_result = split_file(
            m.options.output_dir + '/' + test_name + '.result',
            ignore_errors=True)
        if test_result:
            test_status = test_result[0].split(':')[0]
            if test_status == 'UOK':
                test_metrics['uok'] += 1

    run_testcase = generate_run_testcase('run-test', discs, m.options)

    MainLoop(test_list, run_testcase, collect_result_including_uok, m.options.mainloop_jobs)

    print "Summary: Ran %(run)s/%(total)s tests, with %(failed)s failed, %(crashed)s crashed, %(uok)s unexpectedly passed." % test_metrics

    # Generate the report file
    ReportDiff(m.options.output_dir,
               m.options.old_output_dir).txt_image(m.options.report_file)

    if test_metrics['failed'] > 0 or test_metrics['crashed'] > 0 or test_metrics['uok'] > 0:
        sys.exit(1)


if __name__ == "__main__":
    if os.path.dirname(__file__):
        os.chdir(os.path.dirname(__file__))
    main()
