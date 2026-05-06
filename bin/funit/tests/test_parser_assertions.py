"""Tests for parser assertion directive handling (@assert*, @mpiAssert*)."""

import unittest
import sys

sys.path.append("..")
from funit.pFUnitParser import (
    AtAssert,
    AtAssertAssociated,
    AtAssertNotAssociated,
    AtAssertEqualUserDefined,
    AtAssertEquivalent,
    AtMpiAssert,
)
from funit.tests.parser_test_utils import MockParser


class TestParserAssertions(unittest.TestCase):
    def testMatchAtAssertEqual(self):
        """Check that a line starting with '@assertEqual' is detected
        as an annotation."""
        parser = MockParser([" \n"])
        atAssert = AtAssert(parser)

        self.assertFalse(atAssert.match("@assertEqual"))
        self.assertFalse(atAssert.match("@assertEqual()"))
        self.assertTrue(atAssert.match("@assertEqual(a, b)"))
        self.assertTrue(atAssert.match("@assertequal(a, b)"))  # case insensitive
        self.assertTrue(atAssert.match("@ASSERTEQUAL(a, b)"))  # case insensitive
        self.assertTrue(atAssert.match("@assertEqual(a, b, \"failure message\")"))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssert.apply("   @assertEqual(1, 2, \"failure message\")\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertEqual(1, 2, \"failure message\", &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertAssociated(self):
        """Check that a line starting with '@assertAssociated' is detected
        as an annotation."""
        parser = MockParser([" \n"])
        atAssertAssociated = AtAssertAssociated(parser)

        self.assertFalse(atAssertAssociated.match("@assertAssociated"))
        self.assertFalse(atAssertAssociated.match("@assertAssociated()"))
        self.assertTrue(atAssertAssociated.match("@assertAssociated(a)"))
        self.assertTrue(
            atAssertAssociated.match("@assertassociated(a)")
        )  # case insensitive
        self.assertTrue(
            atAssertAssociated.match("@ASSERTASSOCIATED(a)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertAssociated.apply("   @assertAssociated(a)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(associated(a), &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertAssociatedOverloaded1(self):
        """Check that a line starting with '@assertAssociated' is detected
        as an annotation. atAssertAssociated(a,b) implies a points to b.
        Overriding the name @assertAssociated.
        """
        parser = MockParser([" \n"])
        atAssertAssociated = AtAssertAssociated(parser)

        self.assertFalse(atAssertAssociated.match("@assertAssociated"))
        self.assertFalse(atAssertAssociated.match("@assertAssociated()"))
        self.assertTrue(atAssertAssociated.match("@assertAssociated(a)"))
        self.assertTrue(
            atAssertAssociated.match("@assertassociated(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertAssociated.match("@ASSERTASSOCIATED(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertAssociated.match("@ASSERTASSOCIATED(a_%z(),b)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertAssociated.apply("   @assertAssociated(a,b)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(associated(a,b), &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertAssociatedOverloaded2(self):
        """Check that a line starting with '@assertAssociated' is detected
        as an annotation. atAssertAssociated(a,b) implies a points to b.
        Overriding the name @assertAssociated.
        """
        parser = MockParser([" \n"])
        atAssertAssociated = AtAssertAssociated(parser)

        self.assertFalse(atAssertAssociated.match("@assertAssociated"))
        self.assertFalse(atAssertAssociated.match("@assertAssociated()"))
        self.assertTrue(atAssertAssociated.match("@assertAssociated(a)"))
        self.assertTrue(
            atAssertAssociated.match("@assertassociated(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertAssociated.match("@ASSERTASSOCIATED(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertAssociated.match("@ASSERTASSOCIATED(a_%z(),b)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertAssociated.apply('   @assertAssociated(a,b,message="c")\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual(
            '  call assertTrue(associated(a,b), message="c", &\n', parser.outLines[1]
        )
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertUnAssociated(self):
        """Check that a line starting with '@assertUnAssociated' is detected
        as an annotation."""
        parser = MockParser([" \n"])
        atAssertUnAssociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertUnAssociated.match("@assertUnAssociated"))
        self.assertFalse(atAssertUnAssociated.match("@assertUnAssociated()"))
        self.assertTrue(atAssertUnAssociated.match("@assertUnAssociated(a)"))
        self.assertTrue(
            atAssertUnAssociated.match("@assertunassociated(a)")
        )  # case insensitive
        self.assertTrue(
            atAssertUnAssociated.match("@ASSERTUNASSOCIATED(a)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertUnAssociated.apply("   @assertUnAssociated(a)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a), &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertUnAssociatedWith(self):
        """Check that a line starting with '@assertUnAssociatedWith' is detected
        as an annotation. atAssertUnAssociated(a,b) implies a points to b."""
        parser = MockParser([" \n"])
        atAssertUnAssociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertUnAssociated.match("@assertUnAssociated"))
        self.assertFalse(atAssertUnAssociated.match("@assertUnAssociated()"))
        self.assertTrue(atAssertUnAssociated.match("@assertUnAssociated(a)"))
        self.assertTrue(
            atAssertUnAssociated.match("@assertunassociated(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertUnAssociated.match("@ASSERTUNASSOCIATED(a,b)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertUnAssociated.apply("   @assertUnAssociated(a,b)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a,b), &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertNotassociated(self):
        """Check that a line starting with '@assertNotAssociated' is detected
        as an annotation."""
        parser = MockParser([" \n"])
        atAssertNotassociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertNotassociated.match("@assertNotassociated"))
        self.assertFalse(atAssertNotassociated.match("@assertNotassociated()"))
        self.assertTrue(atAssertNotassociated.match("@assertNotassociated(a)"))
        self.assertTrue(
            atAssertNotassociated.match("@assertnotassociated(a)")
        )  # case insensitive
        self.assertTrue(
            atAssertNotassociated.match("@ASSERTNOTASSOCIATED(a)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertNotassociated.apply("   @assertNotassociated(a)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a), &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertNotassociatedWith(self):
        """Check that a line starting with '@assertNotassociatedWith' is detected
        as an annotation. atAssertNotassociated(a,b) implies a points to b."""
        parser = MockParser([" \n"])
        atAssertNotassociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertNotassociated.match("@assertNotassociated"))
        self.assertFalse(atAssertNotassociated.match("@assertNotassociated()"))
        self.assertTrue(atAssertNotassociated.match("@assertNotassociated(a)"))
        self.assertTrue(
            atAssertNotassociated.match("@assertnotassociated(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertNotassociated.match("@ASSERTNOTASSOCIATED(a,b)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertNotassociated.apply("   @assertNotassociated(a,b)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a,b), &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertEqualUserDefined(self):
        """Check that a line starting with '@assertEqualUserDefined' is detected
        as an annotation. atAssertEqualUserDefined(a,b) implies a points to b."""
        parser = MockParser([" \n"])
        atAssertEqualUserDefined = AtAssertEqualUserDefined(parser)

        self.assertFalse(atAssertEqualUserDefined.match("@assertEqualUserDefined"))
        self.assertFalse(atAssertEqualUserDefined.match("@assertEqualUserDefined()"))
        self.assertFalse(atAssertEqualUserDefined.match("@assertEqualUserDefined(a)"))
        self.assertTrue(
            atAssertEqualUserDefined.match("@assertequaluserdefined(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertEqualUserDefined.match("@ASSERTEQUALUSERDEFINED(a,b)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertEqualUserDefined.apply("   @assertEqualUserDefined(a,b)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(a==b, &\n", parser.outLines[1])
        self.assertEqual(" & message='<a> not equal to <b>', &\n", parser.outLines[2])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[3])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[4])
        self.assertEqual(" & 8)", parser.outLines[5])
        self.assertEqual(" )\n", parser.outLines[6])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[7])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[8])

    def testMatchAtAssertEqualUserDefinedWithMessage(self):
        """Check that a line starting with '@assertEqualUserDefined' is detected
        as an annotation. atAssertEqualUserDefined(a,b) implies a points to b."""
        parser = MockParser([" \n"])
        atAssertEqualUserDefined = AtAssertEqualUserDefined(parser)

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertEqualUserDefined.apply('   @assertEqualUserDefined(a,b,message="c")\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual('  call assertTrue(a==b, message="c", &\n', parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertEquivalent(self):
        """Check that a line starting with '@assertEquivalent' is detected
        as an annotation. atAssertEquivalent(a,b) implies a points to b."""
        parser = MockParser([" \n"])
        atAssertEquivalent = AtAssertEquivalent(parser)

        self.assertFalse(atAssertEquivalent.match("@assertEquivalent"))
        self.assertFalse(atAssertEquivalent.match("@assertEquivalent()"))
        self.assertFalse(atAssertEquivalent.match("@assertEquivalent(a)"))
        self.assertTrue(
            atAssertEquivalent.match("@assertequivalent(a,b)")
        )  # case insensitive
        self.assertTrue(
            atAssertEquivalent.match("@ASSERTEQUIVALENT(a,b)")
        )  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertEquivalent.apply("   @assertEquivalent(a,b)\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(a.eqv.b, &\n", parser.outLines[1])
        self.assertEqual(" & message='<a> not equal to <b>', &\n", parser.outLines[2])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[3])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[4])
        self.assertEqual(" & 8)", parser.outLines[5])
        self.assertEqual(" )\n", parser.outLines[6])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[7])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[8])

    def testMatchAtAssertOther(self):
        """Check that a line starting with '@assert*' is detected
        as an annotation."""
        parser = MockParser([" \n"])
        atAssert = AtAssert(parser)

        self.assertFalse(atAssert.match("@assertTrue"))
        self.assertFalse(atAssert.match("@assertTrue()"))
        self.assertTrue(atAssert.match("@assertTrue(a)"))
        self.assertTrue(atAssert.match("@asserttrue(a)"))  # case insensitive
        self.assertTrue(atAssert.match("@ASSERTTRUE(a)"))  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssert.apply("   @assertTrue(.true.)\n")
        self.assertTrue("#line 8 'foo.pfunit'\n", parser.outLines[0])
        self.assertTrue("  call assertTrue(1, 2, &\n", parser.outLines[1])
        self.assertTrue(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertTrue(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertTrue(" & 8)", parser.outLines[4])
        self.assertTrue(" )\n", parser.outLines[5])
        self.assertTrue("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertTrue("#line 9 'foo.pfunit'\n", parser.outLines[7])

    def testMatchAtMpiAssert(self):
        """Check that a line starting with '@mpiAssert*' is detected
        as an annotation."""
        parser = MockParser(["subroutine foo(this)\n"])
        atMpiAssert = AtMpiAssert(parser)

        self.assertFalse(atMpiAssert.match("@mpiAssertTrue"))
        self.assertFalse(atMpiAssert.match("@mpiAssertTrue()"))
        self.assertTrue(atMpiAssert.match("@mpiAssertTrue(a)"))
        self.assertTrue(atMpiAssert.match("@mpiAssertTrue(a,b)"))
        self.assertTrue(atMpiAssert.match("@mpiasserttrue(a)"))  # case insensitive
        self.assertTrue(atMpiAssert.match("@MPIASSERTTRUE(a)"))  # case insensitive

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atMpiAssert.apply("   @mpiAssertTrue(.true.)\n")
        self.assertTrue("#line 8 'foo.pfunit'\n", parser.outLines[0])
        self.assertTrue("  call assertTrue(1, 2, &\n", parser.outLines[1])
        self.assertTrue(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertTrue(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertTrue(" & 8)", parser.outLines[4])
        self.assertTrue(" )\n", parser.outLines[5])
        self.assertTrue(
            "  if (anyExceptions(this%getMpiCommunicator())) return\n",
            parser.outLines[6],
        )
        self.assertTrue("#line 9 'foo.pfunit'\n", parser.outLines[7])


    def testAssertExceptionRaisedNoAnyExceptionsCheck(self):
        """assertExceptionRaised should NOT emit 'if (anyExceptions()) return'.
        Multiple exceptions may be raised and the user should be able to catch
        each one with successive @assertExceptionRaised directives."""
        parser = MockParser([" \n"])
        atAssert = AtAssert(parser)

        self.assertTrue(atAssert.match("@assertExceptionRaised()"))
        self.assertTrue(atAssert.match("@assertExceptionRaised('some message')"))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssert.apply("   @assertExceptionRaised('msg_x')\n")
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertExceptionRaised('msg_x', &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        # No 'if (anyExceptions()) return' should be emitted
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[6])
        self.assertEqual(6, len(parser.outLines) - 1)  # only 7 lines total (0-6)


if __name__ == "__main__":
    unittest.main()
