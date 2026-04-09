"""Tests for Fortran line continuation handling in parser."""

import io
import unittest
import sys

sys.path.append("..")
from funit.pFUnitParser import Parser
from funit.tests.parser_test_utils import MockWriter


class TestParserContinuation(unittest.TestCase):
    def test_fortran_continuation_in_macro(self):
        """
        Check that macro line continued with '&' is joined properly.
        """
        # Simulated input lines as if read from a Fortran source file
        logical_macro = '@test(timeout = 3.0, foo="long string" &\n'
        logical_macro2 = "    &  , bar=99)\n"
        input_lines = [logical_macro, logical_macro2]
        p = Parser.__new__(Parser)  # bypass __init__
        test_input = "".join(input_lines)
        p.inputFile = io.StringIO(test_input)
        p.currentLineNumber = 0
        p.outputFile = MockWriter(p)
        # Patch isComment to always return False for every input line here
        p.isComment = lambda line: False

        full_line = p.nextLine()
        expected = '@test(timeout = 3.0, foo="long string"   , bar=99)\n'
        self.assertEqual(full_line, expected)

    def test_assert_equal_multiline(self):
        """
        Check that an @assertEqual statement split over multiple lines
        (via Fortran continuation & syntax) is joined correctly.
        """
        input_lines = [
            "@assertEqual(lhs_value, &\n",
            "    &rhs_function(arg1, arg2), &\n",
            '    &"Comparison failed message")\n',
        ]
        combined = "".join(input_lines)
        p = Parser.__new__(Parser)
        p.inputFile = io.StringIO(combined)
        p.currentLineNumber = 0
        p.outputFile = MockWriter(p)
        p.isComment = lambda line: False
        full_line = p.nextLine()
        expected = '@assertEqual(lhs_value, rhs_function(arg1, arg2), "Comparison failed message")\n'
        self.assertEqual(full_line, expected)

    def test_lines_without_continuation_not_collapsed(self):
        """
        Check that separate lines without '&' continuation are returned
        one at a time, not collapsed together.
        """
        input_lines = [
            "module Test_Foo\n",
            "  use funit\n",
            "  implicit none\n",
            "contains\n",
        ]
        combined = "".join(input_lines)
        p = Parser.__new__(Parser)
        p.inputFile = io.StringIO(combined)
        p.currentLineNumber = 0
        p.outputFile = MockWriter(p)
        p.isComment = lambda line: False

        # Each call to nextLine() should return exactly one line
        line1 = p.nextLine()
        self.assertEqual(line1, "module Test_Foo\n")

        line2 = p.nextLine()
        self.assertEqual(line2, "  use funit\n")

        line3 = p.nextLine()
        self.assertEqual(line3, "  implicit none\n")

        line4 = p.nextLine()
        self.assertEqual(line4, "contains\n")

    def test_test_directive_not_collapsed_with_next_line(self):
        """
        Check that @test directive is NOT collapsed with the following
        subroutine line when there's no '&' continuation.
        """
        input_lines = [
            "@test\n",
            "subroutine test_foo()\n",
            "  call do_something()\n",
            "end subroutine test_foo\n",
        ]
        combined = "".join(input_lines)
        p = Parser.__new__(Parser)
        p.inputFile = io.StringIO(combined)
        p.currentLineNumber = 0
        p.outputFile = MockWriter(p)
        p.isComment = lambda line: False

        line1 = p.nextLine()
        self.assertEqual(line1, "@test\n")

        line2 = p.nextLine()
        self.assertEqual(line2, "subroutine test_foo()\n")

        line3 = p.nextLine()
        self.assertEqual(line3, "  call do_something()\n")

        line4 = p.nextLine()
        self.assertEqual(line4, "end subroutine test_foo\n")


if __name__ == "__main__":
    unittest.main()
