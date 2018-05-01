#!/usr/bin/env python3

import unittest
import status_report_stat

class MyUnitTest(unittest.TestCase):

    def test_canruntoplevelscript(self):
        """
        User runs the top level script from command-line like this:
        
          $ python3 status_report_stat.py [STATUS_REPORT_FILE]
        
        According to our style, the top level script will just delegate
        all the works to _run() and pass all the command-line arguments
        to it.
        """

        # sys.argv[0] is the script name
        script_name = 'status_report_stat.py'

        # calling the script without an argument
        argv = (script_name, )

        status_report_stat._run(argv)

    def test_canshowhelpmsg(self):
        """
        If user calls our script without any argument or incorrect
        number of expected arguments, shows a help message.  Like this:

          $ python3 status_report_stat.py
          Usage: python3 status_report_stat.py [STATUS_REPORT_FILE]
        """

        # sys.argv[0] is the script name
        script_name = 'status_report_stat.py'

        # will call the script without an argument
        argv = (script_name, )

        # calling the script
        stdout_msg = status_report_stat._run(argv)

        expected_stdout_msg = \
          'Usage: python3 status_report_stat [STATUS_REPORT_FILE]'

        self.assertEqual(expected_stdout_msg, stdout_msg)

if __name__ == '__main__':
    unittest.main()
