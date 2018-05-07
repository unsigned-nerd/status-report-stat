#!/usr/bin/env python3

def getnoofdays(status_report_file):
    return 3

def _run(argv):
    if len(argv) == 1:
        return 'Usage: python3 status_report_stat [STATUS_REPORT_FILE]'
