#!/usr/bin/env python3

import unittest
import status_report_stat

class MyUnitTest(unittest.TestCase):

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

        # If we call the script with an argument, it will not print a
        # help message.

        argv = (script_name, 'some_argument',)

        # calling the script
        stdout_msg = status_report_stat._run(argv)

        self.assertEqual(None, stdout_msg)

    def test_cancountnoofdays(self):
        """
        test that we can count number of logged days from the status
        report input file
        """

        infile = 'test/status_report.00'

        self.assertEqual(status_report_stat.getnoofdays(infile), 3)

    def test_cancheckifitisataskcategoryline(self):
        """
        test that the specified line is a task category line
        """

        infile = 'test/status_report.00'

        expected_result = (
            False, False, False, False, True,
            False, False, False, False, False,
            False, True, False, False, False,
            False, False, True, False, False,
            False, False, False, False, False,
            False, False, True, False, False,
            False, False, False, False, False,
            False, True, False, False, False,
            False, False, False, False, False,
            False, True, False, False, False,
            False, False, False, False, False,
            False, True, False, False, False,
            False, False, False, False, False,
            False, False, False, True, False,
            False, False, False, False, False,
            False, False, False, True, False,
            False, False, False, False, False,
            False, False, False, True, False,
            False, False, False, False, False,
            False, False
        )

        with open(infile) as infile:
            for index, line in enumerate(infile):
                self.assertEqual(
                        status_report_stat.istaskcategoryline(line),
                        expected_result[index])

        self.fail('Finish the test!')

if __name__ == '__main__':
    unittest.main()
