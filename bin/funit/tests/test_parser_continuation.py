"""Tests for Fortran line continuation handling in parser (issue #537).

Only @-directive lines should have & continuations joined.
Ordinary Fortran continuation lines must be passed through unchanged.
"""

import io
import os
import tempfile
import unittest
import sys

sys.path.append("..")
from funit.pFUnitParser import Parser
from funit.tests.parser_test_utils import MockWriter


def _make_parser_with_input(text):
    """Return a Parser-like object reading from a StringIO, with a MockWriter."""
    p = Parser.__new__(Parser)
    p.inputFile = io.StringIO(text)
    p.currentLineNumber = 0
    p.outLines = []
    p.outputFile = MockWriter(p)
    p.isComment = lambda line: (
        False
    )  # treat all lines as non-comment for nextLine tests
    return p


def _run_full_parser(source, module_name):
    """Write *source* to a named temp file, run the full Parser, return output text."""
    tmpdir = tempfile.mkdtemp()
    in_name = os.path.join(tmpdir, module_name + ".pf")
    out_name = in_name + ".F90"
    try:
        with open(in_name, "w") as f:
            f.write(source)
        p = Parser(in_name, out_name)
        p.run()
        p.final()
        with open(out_name) as f:
            return f.read()
    finally:
        if os.path.exists(in_name):
            os.unlink(in_name)
        if os.path.exists(out_name):
            os.unlink(out_name)
        os.rmdir(tmpdir)


class TestNextLinePassthrough(unittest.TestCase):
    """nextLine() must return one physical line at a time without joining."""

    def test_ordinary_lines_not_joined(self):
        """Separate lines without '&' are returned one at a time."""
        p = _make_parser_with_input(
            "module Test_Foo\n  use funit\n  implicit none\ncontains\n"
        )
        self.assertEqual(p.nextLine(), "module Test_Foo\n")
        self.assertEqual(p.nextLine(), "  use funit\n")
        self.assertEqual(p.nextLine(), "  implicit none\n")
        self.assertEqual(p.nextLine(), "contains\n")

    def test_continuation_line_not_joined_by_nextLine(self):
        """nextLine() must NOT join ordinary Fortran & continuation lines."""
        p = _make_parser_with_input(
            "    vals = [ &\n         1, &\n         2 &\n         ]\n"
        )
        self.assertEqual(p.nextLine(), "    vals = [ &\n")
        self.assertEqual(p.nextLine(), "         1, &\n")
        self.assertEqual(p.nextLine(), "         2 &\n")
        self.assertEqual(p.nextLine(), "         ]\n")

    def test_directive_not_joined_with_next_line_without_ampersand(self):
        """@test (no &) must NOT be joined with the following subroutine line."""
        p = _make_parser_with_input("@test\nsubroutine test_foo()\n")
        self.assertEqual(p.nextLine(), "@test\n")
        self.assertEqual(p.nextLine(), "subroutine test_foo()\n")


class TestJoinContinuationLines(unittest.TestCase):
    """joinContinuationLines() must join & continuations for @-directive lines."""

    def test_single_line_directive_unchanged(self):
        """A directive with no continuation is returned as-is."""
        p = _make_parser_with_input("")  # no more lines needed
        result = p.joinContinuationLines("@assertEqual(1, 2)\n")
        self.assertEqual(result, "@assertEqual(1, 2)\n")

    def test_two_line_directive_joined(self):
        """A directive split across two lines with & is joined correctly."""
        p = _make_parser_with_input("     1)\n")
        result = p.joinContinuationLines("@assertEqual(1, &\n")
        # Leading whitespace on the continuation line is stripped after the optional &
        self.assertEqual(result, "@assertEqual(1, 1)\n")

    def test_three_line_directive_joined(self):
        """A directive split across three lines is fully joined."""
        p = _make_parser_with_input(
            '    &rhs_function(arg1, arg2), &\n    &"Comparison failed")\n'
        )
        result = p.joinContinuationLines("@assertEqual(lhs_value, &\n")
        self.assertEqual(
            result,
            '@assertEqual(lhs_value, rhs_function(arg1, arg2), "Comparison failed")\n',
        )

    def test_leading_ampersand_on_continuation_stripped(self):
        """A leading & on a continuation line is stripped."""
        p = _make_parser_with_input("  &, bar=99)\n")
        result = p.joinContinuationLines('@test(timeout=3.0, foo="long string" &\n')
        self.assertEqual(result, '@test(timeout=3.0, foo="long string" , bar=99)\n')


class TestFullParserContinuationPassthrough(unittest.TestCase):
    """End-to-end tests: ordinary Fortran continuation must survive the full parser."""

    def test_multiline_array_constructor_preserved(self):
        """Multi-line array constructors must NOT be joined into a single line."""
        source = """\
module test_continuation
  use funit
  implicit none
contains
  @test
  subroutine check()
    integer :: vals(3)
    vals = [ &
         1, &
         2, &
         3 &
         ]
    @assertEqual(3, size(vals))
  end subroutine check
end module test_continuation
"""
        output = _run_full_parser(source, "test_continuation")
        self.assertIn("    vals = [ &\n", output)
        self.assertIn("         1, &\n", output)
        self.assertIn("         2, &\n", output)
        self.assertIn("         3 &\n", output)
        self.assertIn("         ]\n", output)

    def test_commented_continuation_line_not_embedded(self):
        """A commented-out element in a continuation block must stay on its own line."""
        source = """\
module test_comment_continuation
  use funit
  implicit none
contains
  @test
  subroutine check()
    integer :: vals(2)
    vals = [ &
         1, &
         ! 99, &
         2 &
         ]
    @assertEqual(2, size(vals))
  end subroutine check
end module test_comment_continuation
"""
        output = _run_full_parser(source, "test_comment_continuation")
        self.assertIn("         ! 99, &\n", output)
        self.assertIn("         1, &\n", output)
        self.assertIn("         2 &\n", output)

    def test_directive_continuation_still_joined(self):
        """@assert directives spanning multiple lines via & must still be processed."""
        source = """\
module test_directive_continuation
  use funit
  implicit none
contains
  @test
  subroutine check()
    @assertEqual(1, &
         1)
  end subroutine check
end module test_directive_continuation
"""
        output = _run_full_parser(source, "test_directive_continuation")
        self.assertIn("call assertEqual(1,", output)


if __name__ == "__main__":
    unittest.main()
