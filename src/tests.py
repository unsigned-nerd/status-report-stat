#!/usr/bin/env python3

import unittest
import status_report_stat

class MyUnitTest(unittest.TestCase):

    def test_canruntoplevelscript(self):
        """
        User runs the top level script from command-line like this:
        
          $ python3 status-report-stat.py [STATUS_REPORT_FILE]
        
        According to our style, the top level script will just delegate
        all the works to _run() and pass all the command-line arguments
        to it.
        """

        # sys.argv[0] is the script name
        script_name = 'status-report-stat.py'

        # calling the script without an argument
        argv = (script_name, )

if __name__ == '__main__':
    unittest.main()
