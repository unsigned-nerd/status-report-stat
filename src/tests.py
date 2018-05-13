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

    def test_cangethoursfromtimeestimationline(self):
        """
        test that we can extract the work hours from the time estimation
        line
        """

        infile = 'test/status_report.00'

        expected_result = (
            None, None, None, None, None,
            None, None, None, 0.5, None,
            None, None, None, None, 2,
            None, None, None, None, None,
            0.5, None, None, None, 0.5,
            None, None, None, None, None,
            None, 2, None, None, None,
            None, None, None, None, 1.5,
            None, None, None, 2, None,
            None, None, None, None, 0.5,
            None, None, None, 0.5, None,
            None, None, None, None, 1,
            None, None, None, 2, None,
            None, None, None, None, None,
            None, 0.5, None, None, None,
            1, None, None, None, None,
            None, 2, None, None, None,
            0.5, None, None, None, None,
            None, 4, None, None, None,
            0.5, None
        )

        with open(infile) as infile:
            for index, line in enumerate(infile):
                self.assertEqual(
                    status_report_stat.getworkhours(line),
                    expected_result[index])

    def test_count_work_hours_for_each_category(self):
        """
        read the file sample/status_report_2018 ,
        and returns:
          non-work: 0.5
          non-computer related work: 2+1.5+0.5
          project management: 0.5+0.5+0.5+0.5+2+0.5
          computer related work: 2+1+2+4+0.5
        """

        infile = 'test/status_report.00'
        expected_result = {
            'non-work': 0.5,
            'non-computer related work': 4,
            'project management': 4.5,
            'computer related work': 9.5
        }

        grouped_work_hours = \
            status_report_stat.getgroupedworkhours(infile)

        self.assertEqual(grouped_work_hours, expected_result)

        self.fail('Finish the test!')

if __name__ == '__main__':
    unittest.main()
