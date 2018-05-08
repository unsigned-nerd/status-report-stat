#!/usr/bin/env python3

import re

def getnoofdays(status_report_file):
    date_line_pattern = re.compile("^\d+/\d+/\d+")
    noofdays = 0
    with open(status_report_file) as infile:
        for line in infile:
            if date_line_pattern.match(line):
                noofdays += 1
    return noofdays

def istaskcategoryline(line):
    task_category_tuple = ('non-work', 'non-computer related work',
        'project management', 'computer related work')
    return line.rstrip() in task_category_tuple

def _run(argv):
    if len(argv) == 1:
        return 'Usage: python3 status_report_stat [STATUS_REPORT_FILE]'
