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
        self.tests = []
        self.mpitests = []
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
        self.assertEqual('foo', getTypeName(' type, extends(something) :: foo'))
        self.assertEqual('foo', getTypeName(' type, abstract :: foo'))
        self.assertEqual('foo', getTypeName(' type, extends(something), abstract :: foo'))

    def testAtTest(self):
        """Check that a line starting with '@test' is detected as an
        annotation."""
        nextLine = 'subroutine myTest()\n'
        parser = MockParser([nextLine])
        atTest = AtTest(parser)
        
        self.assertTrue(atTest.match('@test'))
        self.assertFalse(atTest.match('! @test'))
        self.assertTrue(atTest.match('  @test')) # leading space
        self.assertTrue(atTest.match('@TEST'))   # case insensitive
        self.assertTrue(atTest.match('@Test'))   # mixed case
        self.assertFalse(atTest.match('@Testb')) # can't have trailing characters without whitespace
        self.assertTrue(atTest.match('@Test (b)'))
        self.assertTrue(atTest.match('@Test(b)'))
        self.assertFalse(atTest.match('@Test b')) # next non-space character needs to be '(')
        self.assertTrue(atTest.match('@Test(timeout=3.0)'))
        self.assertFalse(atTest.match('! @Test'))
        self.assertFalse(atTest.match('@assertTrue'))

        atTest.apply('@test\n')
        self.assertEqual('myTest', parser.tests[0]['name'])
        self.assertEqual('!@test\n',parser.outLines[0])
        self.assertEqual(nextLine,parser.outLines[1])

    def testAtTestNoParens(self):
        """Check that test procedure with no parens is accepted."""
        nextLine = 'subroutine myTest ! and a comment \n'
        parser = MockParser([nextLine])
        atTest = AtTest(parser)

        m = atTest.match('@test\n')
        atTest.action(m,'@test\n')
        self.assertEqual('myTest', parser.tests[0]['name'])
        self.assertEqual('!@test\n',parser.outLines[0])
        self.assertEqual(nextLine,parser.outLines[1])

    def testAtTestFail(self):
        """Check that useful error is sent if next line is not properly formatted."""

        nextLine = 'subroutine myTest (] \n' # bad closing paren
        parser = MockParser([nextLine])
        
        with self.assertRaises(MyError):
            atTest = AtTest(parser)
            line = '@test'
            m = atTest.match(line)
            atTest.action(m, line)


    def testAtTestSkipComment(self):
        """Ignore comment lines between @test and subroutine foo()."""
        nextLineA = '! ignore this line \n'
        nextLineB = '\n'
        nextLineC = 'subroutine myTestC()\n'
        parser = MockParser([nextLineA,nextLineB,nextLineC])

        atTest = AtTest(parser)
        atTest.apply('@test\n')
        self.assertEqual('myTestC', parser.tests[0]['name'])
        self.assertEqual('!@test\n',parser.outLines[0])
        self.assertEqual(nextLineC,parser.outLines[1])

    def testAtMpiTest(self):
        """Check that a line starting with '@mpitest' is detected as an
        annotation and that optional parameters are collected."""

        nextLine = 'subroutine myTest(this)\n'
        parser = MockParser([nextLine])
        atMpiTest = AtMpiTest(parser)

        line = '@mpitest(npes=[1])'
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m,line)
        self.assertEqual([1], parser.mpitests[0]['npes'])
        self.assertFalse('ifdef' in parser.mpitests[0])

        # ignore leading space?
        line = '@mpitest( npes=[1])'
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m,line)
        self.assertEqual([1], parser.mpitests[1]['npes'])
        self.assertFalse('ifdef' in parser.mpitests[1])

        line = '@mpitest(npes=[1, 2,3], ifdef=USE_MPI)'
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m,line)
        self.assertEqual([1,2,3], parser.mpitests[2]['npes'])
        self.assertEqual('USE_MPI', parser.mpitests[2]['ifdef'])

        line = '@mpitest(npes=[3],ifdef=USE_MPI)'
        parser.reset()
        m = atMpiTest.match(line)
        self.assertTrue(m)
        atMpiTest.action(m,line)
        self.assertEqual([3], parser.mpitests[3]['npes'])
        self.assertEqual('USE_MPI', parser.mpitests[3]['ifdef'])
        


    def testMatchAtTestCase(self):
        """Check that a line starting with '@testcase' is detected as an
        annotation."""
        nextLine = 'type, extends(TestCase) :: myTestCase\n'
        parser = MockParser([nextLine])
        atTestCase = AtTestCase(parser)

        self.assertTrue(atTestCase.match('@testcase'))
        self.assertTrue(atTestCase.match('  @testcase')) # leading space
        self.assertTrue(atTestCase.match('@TESTCASE'))   # case insensitive
        self.assertTrue(atTestCase.match('@TestCase'))   # mixed case
        self.assertFalse(atTestCase.match('@TestCaseb')) # can't have trailing characters without whitespace
        self.assertFalse(atTestCase.match('@TestCase (b)')) # can't have arguments

        atTestCase.apply('@testCase\n')
        self.assertEqual('myTestCase', parser.testCase)
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
        self.assertTrue(atAssert.match('@assertequal(a, b)')) # case insensitive
        self.assertTrue(atAssert.match('@ASSERTEQUAL(a, b)')) # case insensitive

        parser.fileName = "foo.pfunit"
        parser.lineNumber = 8
        atAssert.apply('   @assertEqual(1, 2)\n')
        self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        self.assertEqual("  call assertEqual(1, 2, &\n", parser.outLines[1])
        self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertEqual(" & 8)", parser.outLines[4])
        self.assertEqual(" )\n", parser.outLines[5])
        self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    def testMatchAtAssertOther(self):
        """Check that a line starting with '@assert*' is detected
        as an annotation."""
        parser = MockParser([' \n'])
        atAssert = AtAssert(parser)

        self.assertFalse(atAssert.match('@assertTrue'))
        self.assertFalse(atAssert.match('@assertTrue()'))
        self.assertTrue(atAssert.match('@assertTrue(a)'))
        self.assertTrue(atAssert.match('@asserttrue(a)')) # case insensitive
        self.assertTrue(atAssert.match('@ASSERTTRUE(a)')) # case insensitive

        parser.fileName = 'foo.pfunit'
        parser.lineNumber = 8
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
        self.assertTrue(atMpiAssert.match('@mpiasserttrue(a)')) # case insensitive
        self.assertTrue(atMpiAssert.match('@MPIASSERTTRUE(a)')) # case insensitive

        parser.fileName = 'foo.pfunit'
        parser.lineNumber = 8
        atMpiAssert.apply('   @mpiAssertTrue(.true.)\n')
        self.assertTrue("#line 8 'foo.pfunit'\n", parser.outLines[0])
        self.assertTrue("  call assertTrue(1, 2, &\n", parser.outLines[1])
        self.assertTrue(" & location=SourceLocation( &\n", parser.outLines[2])
        self.assertTrue(" & 'foo.pfunit', &\n", parser.outLines[3])
        self.assertTrue(" & 8)", parser.outLines[4])
        self.assertTrue(" )\n", parser.outLines[5])
        self.assertTrue("  if (anyExceptions(this%getMpiCommunicator())) return\n", parser.outLines[6])
        self.assertTrue("#line 9 'foo.pfunit'\n", parser.outLines[7])

    def testMatchAtBefore(self):
        """Check that a line starting with '@before*' ...""" 
        procedure = 'mySetUp'
        nextLine = 'subroutine ' + procedure +'()\n'
        parser = MockParser([nextLine])
        atBefore = AtBefore(parser)
        self.assertTrue(atBefore.match('  @before'))
        self.assertFalse(atBefore.match('  @beforeb'))

        atBefore.apply('@before\n')
        self.assertEqual(procedure, parser.setUp)
        self.assertEqual('!@before\n', parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])


    def testMatchAtAfter(self):
        """Check that a line starting with '@after*' ...""" 
        procedure = 'myTearDown'
        nextLine = 'subroutine ' + procedure +'()\n'
        parser = MockParser([nextLine])
        atAfter = AtAfter(parser)
        self.assertTrue(atAfter.match('  @after'))
        self.assertFalse(atAfter.match('  @afterb'))

        atAfter.apply('@after\n')
        self.assertEqual(procedure, parser.tearDown)
        self.assertEqual('!@after\n', parser.outLines[0])
        self.assertEqual(nextLine, parser.outLines[1])

    def testMatchAtSuite(self):
        """Check that a line starting with '@suite changes the suite name ...""" 
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
