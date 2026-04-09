"""Tests for parser directive handling (@test, @before, @after, @suite, @testcase, @mpitest)."""

import unittest
import sys

sys.path.append("..")
from funit.pFUnitParser import (
    AtTest,
    AtMpiTest,
    AtTestCase,
    AtBefore,
    AtAfter,
    AtSuite,
    IsTestMethod,
    MyError,
)
from funit.tests.parser_test_utils import MockParser


class TestParserDirectives(unittest.TestCase):
    def testAtTest(self):
        """Check that a line starting with '@test' is detected as an
        annotation."""
        nextLine = "subroutine myTest()\n"
        parser = MockParser([nextLine])
        atTest = AtTest(parser)
        is_test_method = IsTestMethod(parser)

        self.assertTrue(atTest.match("@test"))
        self.assertFalse(atTest.match("! @test"))
        self.assertTrue(atTest.match("  @test"))  # leading space
        self.assertTrue(atTest.match("@TEST"))  # case insensitive
        self.assertTrue(atTest.match("@Test"))  # mixed case
        self.assertFalse(
            atTest.match("@Testb")
        )  # can't have trailing characters without whitespace
        self.assertTrue(atTest.match("@Test (b)"))
        self.assertTrue(atTest.match("@Test(b)"))
        self.assertFalse(
            atTest.match("@Test b")
        )  # next non-space character needs to be '(')
        self.assertTrue(atTest.match("@Test(timeout=3.0)"))
        self.assertFalse(atTest.match("! @Test"))
        self.assertFalse(atTest.match("@assertTrue"))
        self.assertTrue(atTest.match("@test(cases = [1,3,5])"))

        atTest.apply("@test\n")
        is_test_method.apply(nextLine)
        self.assertEqual("myTest", parser.userTestMethods[0]["name"])
        self.assertEqual("!@test\n", parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testAtTestNoParens(self):
        """Check that test procedure with no parens is accepted."""
        nextLine = "subroutine myTest ! and a comment \n"
        parser = MockParser([nextLine])
        atTest = AtTest(parser)
        is_test_method = IsTestMethod(parser)

        m = atTest.match("@test\n")
        atTest.action(m, "@test\n")
        is_test_method.apply(nextLine)
        self.assertEqual("myTest", parser.userTestMethods[0]["name"])
        self.assertEqual("!@test\n", parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    @unittest.skip("I don't think this test can pass with the new multi-stage parsing")
    def testAtTestFail(self):
        """Check that useful error is sent if next line is not properly formatted."""

        nextLine = "subroutine myTest (] \n"  # bad closing paren
        parser = MockParser([nextLine])

        with self.assertRaises(MyError):
            atTest = AtTest(parser)
            line = "@test"
            m = atTest.match(line)
            atTest.action(m, line)

            is_test_method = IsTestMethod(parser)
            is_test_method.apply(nextLine)

    def testAtTestSkipComment(self):
        """Ignore comment lines between @test and subroutine foo()."""
        nextLineA = "! ignore this line \n"
        nextLineB = "\n"
        nextLineC = "subroutine myTestC()\n"
        parser = MockParser([nextLineA, nextLineB, nextLineC])

        atTest = AtTest(parser)
        atTest.apply("@test\n")
        is_test_method = IsTestMethod(parser)
        is_test_method.apply(nextLineA)
        is_test_method.apply(nextLineB)
        is_test_method.apply(nextLineC)
        self.assertEqual("myTestC", parser.userTestMethods[0]["name"])
        self.assertEqual("!@test\n", parser.outLines[0])
        self.assertEqual(nextLineC, parser.outLines[1])

    def testAtMpiTest(self):
        """Check that a line starting with '@mpitest' is detected as an
        annotation and that optional parameters are collected."""

        nextLine = "subroutine myTest(this)\n"
        parser = MockParser([nextLine])
        atMpiTest = AtMpiTest(parser)
        is_test_method = IsTestMethod(parser)

        line = "@mpitest(npes=[1])"
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        is_test_method.apply(nextLine)
        self.assertEqual([1], parser.userTestMethods[0]["npRequests"])
        self.assertFalse("ifdef" in parser.userTestMethods[0])

        # ignore leading space?
        line = "@mpitest( npes=[1])"
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        is_test_method.apply(nextLine)
        self.assertEqual([1], parser.userTestMethods[1]["npRequests"])
        self.assertFalse("ifdef" in parser.userTestMethods[1])

        line = "@mpitest(npes=[1, 2,3], ifdef=USE_MPI)"
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        is_test_method.apply(nextLine)
        self.assertEqual([1, 2, 3], parser.userTestMethods[2]["npRequests"])
        self.assertEqual("USE_MPI", parser.userTestMethods[2]["ifdef"])

        line = "@mpitest(npes=[3],ifdef=USE_MPI)"
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        is_test_method.apply(nextLine)
        self.assertEqual([3], parser.userTestMethods[3]["npRequests"])
        self.assertEqual("USE_MPI", parser.userTestMethods[3]["ifdef"])

    def testMatchAtTestCase(self):
        """Check that a line starting with '@testcase' is detected as an
        annotation."""
        nextLine = "type, extends(TestCase) :: myTestCase\n"
        parser = MockParser([nextLine])
        atTestCase = AtTestCase(parser)

        self.assertTrue(atTestCase.match("@testcase"))
        self.assertTrue(atTestCase.match("  @testcase"))  # leading space
        self.assertTrue(atTestCase.match("@TESTCASE"))  # case insensitive
        self.assertTrue(atTestCase.match("@TestCase"))  # mixed case
        self.assertFalse(
            atTestCase.match("@TestCaseb")
        )  # can't have trailing characters without whitespace

        atTestCase.apply("@testCase\n")
        self.assertEqual("myTestCase", parser.userTestCase["type"])
        self.assertEqual("!@testCase\n", parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testMatchAtBefore(self):
        """Check that a line starting with '@before*' ..."""
        procedure = "mySetUp"
        nextLine = "subroutine " + procedure + "()\n"
        parser = MockParser([nextLine])
        atBefore = AtBefore(parser)
        self.assertTrue(atBefore.match("  @before"))
        self.assertFalse(atBefore.match("  @beforeb"))

        atBefore.apply("@before\n")
        self.assertEqual(procedure, parser.userTestCase["setUp"])
        self.assertEqual("!@before\n", parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testMatchAtAfter(self):
        """Check that a line starting with '@after*' ..."""
        procedure = "myTearDown"
        nextLine = "subroutine " + procedure + "()\n"
        parser = MockParser([nextLine])
        atAfter = AtAfter(parser)
        self.assertTrue(atAfter.match("  @after"))
        self.assertFalse(atAfter.match("  @afterb"))

        atAfter.apply("@after\n")
        self.assertEqual(procedure, parser.userTestCase["tearDown"])
        self.assertEqual("!@after\n", parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testMatchAtSuite(self):
        """Check that a line starting with '@suite changes the suite name ..."""
        parser = MockParser(["\n"])
        atSuite = AtSuite(parser)
        self.assertTrue(atSuite.match("  @suite (name='a')"))
        self.assertTrue(atSuite.match("  @suite (name=a)"))
        self.assertTrue(atSuite.match("  @suite(name='aa')"))
        self.assertFalse(atSuite.match("  @suite(name=a b)"))
        self.assertFalse(atSuite.match("  @suiteb()"))
        self.assertFalse(atSuite.match("  @suite()"))

        atSuite.apply("@suite ( name =  'mySuite')\n")
        self.assertEqual("mySuite", parser.suiteName)


if __name__ == "__main__":
    unittest.main()
