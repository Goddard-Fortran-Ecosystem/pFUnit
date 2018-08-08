import unittest
import sys
sys.path.append('..')
from pFUnitParser import *


class MockWriter():
    def __init__(self, parser):
        self.parser = parser

    def write(self, line):
        self.parser.outLines.append(line)


class MockParser(Parser):
    def __init__(self, lines):
        self.saveLines = lines
        self.lines = self.saveLines[:]
        self.outputFile = MockWriter(self)
        self.outLines = []
        self.userTestCase = {}
        self.userTestMethods = []
        self.currentSelfObjectName = ''

    def nextLine(self):
        while True:
            line = self.lines.pop(0)
            if (self.isComment(line)):
                pass
            else:
                break
        return line

    def reset(self):
        self.lines = self.saveLines[:]


class TestParseLine(unittest.TestCase):

    def testCppSetLineAndFile(self):
        self.assertEqual('#line 7 "foo"\n', cppSetLineAndFile(7, 'foo'))
        self.assertEqual('#line 3 "bar"\n', cppSetLineAndFile(3, 'bar'))

    def testGetSubroutineName(self):
        self.assertEqual('a', getSubroutineName('subroutine a()'))
        self.assertEqual('abcd', getSubroutineName('subroutine   abcd ()'))

    def testGetSelfObjectName(self):
        self.assertEqual('b', getSelfObjectName('subroutine a(b)'))
        self.assertEqual('bc', getSelfObjectName('subroutine a(bc)'))
        self.assertEqual('bc', getSelfObjectName('subroutine a(bc,d)'))
        self.assertEqual('bc', getSelfObjectName('subroutine a(bc, d)'))
        self.assertEqual('bc', getSelfObjectName('subroutine a(bc ,d)'))
        self.assertEqual('bc', getSelfObjectName('subroutine a(bc , d)'))

    def testGetTypeName(self):
        self.assertEqual('foo', getTypeName(' type :: foo'))
        self.assertEqual('foo',
                         getTypeName(' type, extends(something) :: foo'))
        self.assertEqual('foo', getTypeName(' type, abstract :: foo'))
        result = getTypeName(' type, extends(something), abstract :: foo')
        self.assertEqual('foo', result)

    def testAtTest(self):
        """Check that a line starting with '@test' is detected as an
        annotation."""
        nextLine = 'subroutine myTest()\n'
        parser = MockParser([nextLine])
        atTest = AtTest(parser)

        self.assertTrue(atTest.match('@test'))
        self.assertFalse(atTest.match('! @test'))
        self.assertTrue(atTest.match('  @test'))  # leading space
        self.assertTrue(atTest.match('@TEST'))    # case insensitive
        self.assertTrue(atTest.match('@Test'))    # mixed case
        # Can't have trailing characters without whitespace
        self.assertFalse(atTest.match('@Testb'))
        self.assertTrue(atTest.match('@Test (b)'))
        self.assertTrue(atTest.match('@Test(b)'))
        # Next non-space character needs to be '(')
        self.assertFalse(atTest.match('@Test b'))
        self.assertTrue(atTest.match('@Test(timeout=3.0)'))
        self.assertFalse(atTest.match('! @Test'))
        self.assertFalse(atTest.match('@assertTrue'))
        self.assertTrue(atTest.match('@test(cases = [1,3,5])'))

        atTest.apply('@test\n')
        self.assertEqual('myTest', parser.userTestMethods[0]['name'])
        self.assertEqual('!@test\n', parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testAtTestNoParens(self):
        """Check that test procedure with no parens is accepted."""
        nextLine = 'subroutine myTest ! and a comment \n'
        parser = MockParser([nextLine])
        atTest = AtTest(parser)

        m = atTest.match('@test\n')
        atTest.action(m, '@test\n')
        self.assertEqual('myTest', parser.userTestMethods[0]['name'])
        self.assertEqual('!@test\n', parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testAtTestFail(self):
        """
        Check that useful error is sent if next line is not properly formatted.
        """
        nextLine = 'subroutine myTest (] \n'  # bad closing paren
        parser = MockParser([nextLine])

        try:
            atTest = AtTest(parser)
            line = '@test'
            m = atTest.match(line)
            atTest.action(m, line)
            self.assertTrue(False, msg='Expected assertion was not raised')
        except MyError:
            pass  # Correct response

    def testAtTestSkipComment(self):
        """Ignore comment lines between @test and subroutine foo()."""
        nextLineA = '! ignore this line \n'
        nextLineB = '\n'
        nextLineC = 'subroutine myTestC()\n'
        parser = MockParser([nextLineA, nextLineB, nextLineC])

        atTest = AtTest(parser)
        atTest.apply('@test\n')
        self.assertEqual('myTestC', parser.userTestMethods[0]['name'])
        self.assertEqual('!@test\n', parser.outLines[0])
        self.assertEqual(nextLineC, parser.outLines[1])

    def testAtMpiTest(self):
        """Check that a line starting with '@mpitest' is detected as an
        annotation and that optional parameters are collected."""

        nextLine = 'subroutine myTest(this)\n'
        parser = MockParser([nextLine])
        atMpiTest = AtMpiTest(parser)

        line = '@mpitest(npes=[1])'
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        self.assertEqual([1], parser.userTestMethods[0]['npRequests'])
        self.assertFalse('ifdef' in parser.userTestMethods[0])

        # ignore leading space?
        line = '@mpitest( npes=[1])'
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        self.assertEqual([1], parser.userTestMethods[1]['npRequests'])
        self.assertFalse('ifdef' in parser.userTestMethods[1])

        line = '@mpitest(npes=[1, 2,3], ifdef=USE_MPI)'
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        self.assertEqual([1, 2, 3], parser.userTestMethods[2]['npRequests'])
        self.assertEqual('USE_MPI', parser.userTestMethods[2]['ifdef'])

        line = '@mpitest(npes=[3],ifdef=USE_MPI)'
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m, line)
        self.assertEqual([3], parser.userTestMethods[3]['npRequests'])
        self.assertEqual('USE_MPI', parser.userTestMethods[3]['ifdef'])

    def testMatchAtTestCase(self):
        """Check that a line starting with '@testcase' is detected as an
        annotation."""
        nextLine = 'type, extends(TestCase) :: myTestCase\n'
        parser = MockParser([nextLine])
        atTestCase = AtTestCase(parser)

        self.assertTrue(atTestCase.match('@testcase'))
        self.assertTrue(atTestCase.match('  @testcase'))  # leading space
        self.assertTrue(atTestCase.match('@TESTCASE'))    # case insensitive
        self.assertTrue(atTestCase.match('@TestCase'))    # mixed case
        # Can't have trailing characters without whitespace
        self.assertFalse(atTestCase.match('@TestCaseb'))

        atTestCase.apply('@testCase\n')
        self.assertEqual('myTestCase', parser.userTestCase['type'])
        self.assertEqual('!@testCase\n', parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testMatchAtAssertEqual(self):
        """Check that a line starting with '@assertEqual' is detected
        as an annotation."""
        parser = MockParser([' \n'])
        atAssert = AtAssert(parser)

        self.assertFalse(atAssert.match('@assertEqual'))
        self.assertFalse(atAssert.match('@assertEqual()'))
        self.assertTrue(atAssert.match('@assertEqual(a, b)'))
        # Case insensitive
        self.assertTrue(atAssert.match('@assertequal(a, b)'))
        self.assertTrue(atAssert.match('@ASSERTEQUAL(a, b)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssert.apply('   @assertEqual(1, 2)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertEqual(1, 2, &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testParseArgsFirstRest(self):
        """Test that the first-rest argument parsing is adequate."""
        self.assertEqual(['a1', 'b1'],
                         parseArgsFirstRest('', 'a1,b1'))
        self.assertEqual(['a4()', 'b4'],
                         parseArgsFirstRest('', 'a4(),b4'))
        self.assertEqual(['a4%z()', 'b4'],
                         parseArgsFirstRest('', 'a4%z(),b4'))
        self.assertEqual(['a4', 'b4%z()'],
                         parseArgsFirstRest('', 'a4,b4%z()'))
        self.assertEqual(['a10', 'b10,c10'],
                         parseArgsFirstRest('', 'a10,b10,c10'))
        self.assertEqual(['a2'],
                         parseArgsFirstRest("@assertassociated",
                                            "@assertassociated(a2)"))
        fragment = "@assertassociated(a3,b3,message='This is the message.')"
        result = parseArgsFirstRest("@assertassociated", fragment)
        self.assertEqual(['a3', "b3,message='This is the message.'"], result)
        self.assertEqual(['a', 'b,c,d'],
                         parseArgsFirstRest("@assertassociated",
                                            "@assertassociated(a,b,c,d)"))

    def testParseArgsFirstSecondRest(self):
        """Test that the first-second-rest argument parsing is adequate."""
        self.assertEqual(None,
                         parseArgsFirstSecondRest("@assertassociated",
                                                  "@assertassociated"))
        self.assertEqual(None,
                         parseArgsFirstSecondRest("@assertassociated",
                                                  "@assertassociated()"))
        self.assertEqual(['a'],
                         parseArgsFirstSecondRest("@assertassociated",
                                                  "@assertassociated(a)"))
        self.assertEqual(['a', 'b'],
                         parseArgsFirstSecondRest("@assertassociated",
                                                  "@assertassociated(a,b)"))
        fragment = "@assertassociated(a,b,message='This is the message.')"
        result = parseArgsFirstSecondRest("@assertassociated", fragment)
        self.assertEqual(['a', 'b', "message='This is the message.'"], result)
        result = parseArgsFirstSecondRest("@assertassociated",
                                          "@assertassociated(a,b%z(),c,d)")
        self.assertEqual(['a', 'b%z()', 'c,d'], result)
        self.assertEqual(['a4', 'b4', 'c4'],
                         parseArgsFirstSecondRest('', 'a4,b4,c4'))

    def testMatchAtAssertAssociated(self):
        """Check that a line starting with '@assertAssociated' is detected
        as an annotation."""
        parser = MockParser([' \n'])
        atAssertAssociated = AtAssertAssociated(parser)

        self.assertFalse(atAssertAssociated.match('@assertAssociated'))
        self.assertFalse(atAssertAssociated.match('@assertAssociated()'))
        self.assertTrue(atAssertAssociated.match('@assertAssociated(a)'))
        # Case insensitive
        self.assertTrue(atAssertAssociated.match('@assertassociated(a)'))
        self.assertTrue(atAssertAssociated.match('@ASSERTASSOCIATED(a)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertAssociated.apply('   @assertAssociated(a)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(associated(a), &\n",
                         parser.outLines[1])
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
        parser = MockParser([' \n'])
        atAssertAssociated = AtAssertAssociated(parser)

        self.assertFalse(atAssertAssociated.match('@assertAssociated'))
        self.assertFalse(atAssertAssociated.match('@assertAssociated()'))
        self.assertTrue(atAssertAssociated.match('@assertAssociated(a)'))
        # Case insensitive
        self.assertTrue(atAssertAssociated.match('@assertassociated(a,b)'))
        self.assertTrue(atAssertAssociated.match('@ASSERTASSOCIATED(a,b)'))
        self.assertTrue(
            atAssertAssociated.match('@ASSERTASSOCIATED(a_%z(),b)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertAssociated.apply('   @assertAssociated(a,b)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(associated(a,b), &\n",
                         parser.outLines[1])
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
        parser = MockParser([' \n'])
        atAssertAssociated = AtAssertAssociated(parser)

        self.assertFalse(atAssertAssociated.match('@assertAssociated'))
        self.assertFalse(atAssertAssociated.match('@assertAssociated()'))
        self.assertTrue(atAssertAssociated.match('@assertAssociated(a)'))
        # Case insensitive
        self.assertTrue(atAssertAssociated.match('@assertassociated(a,b)'))
        self.assertTrue(atAssertAssociated.match('@ASSERTASSOCIATED(a,b)'))
        self.assertTrue(
            atAssertAssociated.match('@ASSERTASSOCIATED(a_%z(),b)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertAssociated.apply('   @assertAssociated(a,b,message="c")\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual('  call assertTrue(associated(a,b), message="c", &\n',
                         parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertUnAssociated(self):
        """Check that a line starting with '@assertUnAssociated' is detected
        as an annotation."""
        parser = MockParser([' \n'])
        atAssertUnAssociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertUnAssociated.match('@assertUnAssociated'))
        self.assertFalse(atAssertUnAssociated.match('@assertUnAssociated()'))
        self.assertTrue(atAssertUnAssociated.match('@assertUnAssociated(a)'))
        # Case insensitive
        self.assertTrue(atAssertUnAssociated.match('@assertunassociated(a)'))
        self.assertTrue(atAssertUnAssociated.match('@ASSERTUNASSOCIATED(a)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertUnAssociated.apply('   @assertUnAssociated(a)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a), &\n",
                         parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertUnAssociatedWith(self):
        """
        Check that a line starting with '@assertUnAssociatedWith' is detected
        as an annotation. atAssertUnAssociated(a,b) implies a points to b.
        """
        parser = MockParser([' \n'])
        atAssertUnAssociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertUnAssociated.match('@assertUnAssociated'))
        self.assertFalse(atAssertUnAssociated.match('@assertUnAssociated()'))
        self.assertTrue(atAssertUnAssociated.match('@assertUnAssociated(a)'))
        # Case insensitive
        self.assertTrue(atAssertUnAssociated.match('@assertunassociated(a,b)'))
        self.assertTrue(atAssertUnAssociated.match('@ASSERTUNASSOCIATED(a,b)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertUnAssociated.apply('   @assertUnAssociated(a,b)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a,b), &\n",
                         parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertNotassociated(self):
        """Check that a line starting with '@assertNotAssociated' is detected
        as an annotation."""
        parser = MockParser([' \n'])
        atAssertNotassociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertNotassociated.match('@assertNotassociated'))
        self.assertFalse(atAssertNotassociated.match('@assertNotassociated()'))
        self.assertTrue(atAssertNotassociated.match('@assertNotassociated(a)'))
        # Case insensitive
        self.assertTrue(atAssertNotassociated.match('@assertnotassociated(a)'))
        self.assertTrue(atAssertNotassociated.match('@ASSERTNOTASSOCIATED(a)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertNotassociated.apply('   @assertNotassociated(a)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a), &\n",
                         parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertNotassociatedWith(self):
        """
        Check that a line starting with '@assertNotassociatedWith' is detected
        as an annotation. atAssertNotassociated(a,b) implies a points to b.
        """
        parser = MockParser([' \n'])
        atAssertNotassociated = AtAssertNotAssociated(parser)

        self.assertFalse(atAssertNotassociated.match('@assertNotassociated'))
        self.assertFalse(atAssertNotassociated.match('@assertNotassociated()'))
        self.assertTrue(atAssertNotassociated.match('@assertNotassociated(a)'))
        # Case insensitive
        self.assertTrue(
            atAssertNotassociated.match('@assertnotassociated(a,b)'))
        self.assertTrue(
            atAssertNotassociated.match('@ASSERTNOTASSOCIATED(a,b)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertNotassociated.apply('   @assertNotassociated(a,b)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertFalse(associated(a,b), &\n",
                         parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertEqualUserDefined(self):
        """
        Check that a line starting with '@assertEqualUserDefined' is detected
        as an annotation. atAssertEqualUserDefined(a,b) implies a points to b.
        """
        parser = MockParser([' \n'])
        atAssertEqualUserDefined = AtAssertEqualUserDefined(parser)

        self.assertFalse(
            atAssertEqualUserDefined.match('@assertEqualUserDefined'))
        self.assertFalse(
            atAssertEqualUserDefined.match('@assertEqualUserDefined()'))
        self.assertFalse(
            atAssertEqualUserDefined.match('@assertEqualUserDefined(a)'))
        # Case insensitive
        self.assertTrue(
            atAssertEqualUserDefined.match('@assertequaluserdefined(a,b)'))
        self.assertTrue(
            atAssertEqualUserDefined.match('@ASSERTEQUALUSERDEFINED(a,b)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertEqualUserDefined.apply('   @assertEqualUserDefined(a,b)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(a==b, &\n", parser.outLines[1])
        self.assertEqual(" & message='<a> not equal to <b>', &\n",
                         parser.outLines[2])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[3])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[4])
        self.assertEqual(" & 8)", parser.outLines[5])
        self.assertEqual(" )\n", parser.outLines[6])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[7])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[8])

    def testMatchAtAssertEqualUserDefinedWithMessage(self):
        """
        Check that a line starting with '@assertEqualUserDefined' is detected
        as an annotation. atAssertEqualUserDefined(a,b) implies a points to b.
        """
        parser = MockParser([' \n'])
        atAssertEqualUserDefined = AtAssertEqualUserDefined(parser)

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        text = '   @assertEqualUserDefined(a,b,message="c")\n'
        atAssertEqualUserDefined.apply(text)
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual('  call assertTrue(a==b, message="c", &\n',
                         parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertEquivalent(self):
        """Check that a line starting with '@assertEquivalent' is detected
        as an annotation. atAssertEquivalent(a,b) implies a points to b."""
        parser = MockParser([' \n'])
        atAssertEquivalent = AtAssertEquivalent(parser)

        self.assertFalse(atAssertEquivalent.match('@assertEquivalent'))
        self.assertFalse(atAssertEquivalent.match('@assertEquivalent()'))
        self.assertFalse(atAssertEquivalent.match('@assertEquivalent(a)'))
        # Case insensitive
        self.assertTrue(atAssertEquivalent.match('@assertequivalent(a,b)'))
        self.assertTrue(atAssertEquivalent.match('@ASSERTEQUIVALENT(a,b)'))

        parser.fileName = "foo.pfunit"
        parser.currentLineNumber = 8
        atAssertEquivalent.apply('   @assertEquivalent(a,b)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertTrue(a.eqv.b, &\n", parser.outLines[1])
        self.assertEqual(" & message='<a> not equal to <b>', &\n",
                         parser.outLines[2])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[3])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[4])
        self.assertEqual(" & 8)", parser.outLines[5])
        self.assertEqual(" )\n", parser.outLines[6])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[7])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[8])

    def testMatchAtAssertOther(self):
        """Check that a line starting with '@assert*' is detected
        as an annotation."""
        parser = MockParser([' \n'])
        atAssert = AtAssert(parser)

        self.assertFalse(atAssert.match('@assertTrue'))
        self.assertFalse(atAssert.match('@assertTrue()'))
        self.assertTrue(atAssert.match('@assertTrue(a)'))
        # Case insensitive
        self.assertTrue(atAssert.match('@asserttrue(a)'))
        self.assertTrue(atAssert.match('@ASSERTTRUE(a)'))

        parser.fileName = 'foo.pfunit'
        parser.currentLineNumber = 8
        atAssert.apply('   @assertTrue(.true.)\n')
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
        parser = MockParser(['subroutine foo(this)\n'])
        atMpiAssert = AtMpiAssert(parser)

        self.assertFalse(atMpiAssert.match('@mpiAssertTrue'))
        self.assertFalse(atMpiAssert.match('@mpiAssertTrue()'))
        self.assertTrue(atMpiAssert.match('@mpiAssertTrue(a)'))
        self.assertTrue(atMpiAssert.match('@mpiAssertTrue(a,b)'))
        # Case insensitive
        self.assertTrue(atMpiAssert.match('@mpiasserttrue(a)'))
        self.assertTrue(atMpiAssert.match('@MPIASSERTTRUE(a)'))

        parser.fileName = 'foo.pfunit'
        parser.currentLineNumber = 8
        atMpiAssert.apply('   @mpiAssertTrue(.true.)\n')
        self.assertTrue("#line 8 'foo.pfunit'\n", parser.outLines[0])
        self.assertTrue("  call assertTrue(1, 2, &\n", parser.outLines[1])
        self.assertTrue(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertTrue(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertTrue(" & 8)", parser.outLines[4])
        self.assertTrue(" )\n", parser.outLines[5])
        expected = "  if (anyExceptions(this%getMpiCommunicator())) return\n"
        self.assertTrue(expected, parser.outLines[6])
        self.assertTrue("#line 9 'foo.pfunit'\n", parser.outLines[7])

    def testMatchAtBefore(self):
        """Check that a line starting with '@before*' ..."""
        procedure = 'mySetUp'
        nextLine = 'subroutine ' + procedure + '()\n'
        parser = MockParser([nextLine])
        atBefore = AtBefore(parser)
        self.assertTrue(atBefore.match('  @before'))
        self.assertFalse(atBefore.match('  @beforeb'))

        atBefore.apply('@before\n')
        self.assertEqual(procedure, parser.userTestCase['setUp'])
        self.assertEqual('!@before\n', parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testMatchAtAfter(self):
        """Check that a line starting with '@after*' ..."""
        procedure = 'myTearDown'
        nextLine = 'subroutine ' + procedure + '()\n'
        parser = MockParser([nextLine])
        atAfter = AtAfter(parser)
        self.assertTrue(atAfter.match('  @after'))
        self.assertFalse(atAfter.match('  @afterb'))

        atAfter.apply('@after\n')
        self.assertEqual(procedure, parser.userTestCase['tearDown'])
        self.assertEqual('!@after\n', parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testMatchAtSuite(self):
        """
        Check that a line starting with '@suite changes the suite name ...
        """
        parser = MockParser(['\n'])
        atSuite = AtSuite(parser)
        self.assertTrue(atSuite.match("  @suite (name='a')"))
        self.assertTrue(atSuite.match("  @suite (name=""a"")"))
        self.assertTrue(atSuite.match("  @suite(name='aa')"))
        self.assertFalse(atSuite.match("  @suite(name=a b)"))
        self.assertFalse(atSuite.match("  @suiteb()"))
        self.assertFalse(atSuite.match("  @suite()"))

        atSuite.apply("@suite ( name =  'mySuite')\n")
        self.assertEqual('mySuite', parser.suiteName)


if __name__ == "__main__":
    unittest.main()
