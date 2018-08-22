from abc import ABCMeta, abstractmethod
import io
import unittest

import funit.parser

class Test_Scanner(unittest.TestCase):
    def setUp(self):
        self._source = io.StringIO()
        self._source.name = 'test_input'
        self._target = io.StringIO()
        self._target.name = 'test_output'

    def tearDown(self):
        self._target.close()
        self._source.close()

    def test_line(self):
        print('First line\nSecond line\n', file=self._source, end='')
        self._source.seek(0)
        parameters = funit.parser.Parameters()
        unit_under_test = funit.parser._Scanner(self._source, self._target,
                                                parameters)
        self.assertFalse(unit_under_test.done())
        unit_under_test.give_line('Teapot brewfest\n')
        self.assertEqual('Teapot brewfest\n', unit_under_test.take_line())
        self.assertEqual('First line\n', unit_under_test.take_line())
        unit_under_test.give_line('Beef Haddock-breath\n')
        self.assertEqual('Beef Haddock-breath\n', unit_under_test.take_line())
        self.assertEqual('Second line\n', unit_under_test.take_line())
        self.assertTrue(unit_under_test.done())
        unit_under_test.give_line('Argle bargle\n')
        self.assertFalse(unit_under_test.done())
        self.assertEqual('Argle bargle\n', unit_under_test.take_line())
        self.assertIsNone(unit_under_test.take_line())
        self.assertTrue(unit_under_test.done())

    def test_line_directives(self):
        for switch, directive in ((False, 'line'), (True, '')):
            with self.subTest(switch=switch, directive=directive):
                expected = '''#{directive} 0 "test_input"
#{directive} 1 "test_input"
#{directive} 2 "test_input"
#{directive} 4 "test_input"
#{directive} 3 "test_input"
'''.format(directive=directive)

                print('Alpha\nBravo\nCharly\nDelta\n',
                      file=self._source, end='')
                self._source.seek(0)
                self._target = io.StringIO()
                self._target.name = 'test_output'
                parameters = funit.parser.Parameters()
                unit_under_test = funit.parser._Scanner(self._source,
                                                        self._target,
                                                        parameters,
                                                        linemarkers=switch)
                # line 0
                unit_under_test.emmit_linemarker()
                unit_under_test.take_line()
                # line 1
                unit_under_test.emmit_linemarker()
                unit_under_test.emmit_linemarker(1)
                unit_under_test.take_line()
                # line 2
                unit_under_test.emmit_linemarker(2)
                unit_under_test.take_line()
                # line 3
                unit_under_test.emmit_linemarker()

                self.assertMultiLineEqual(expected, self._target.getvalue())

    class _DummyState(funit.parser._State):
        pass

    def test_state(self):
        parameters = funit.parser.Parameters()
        unit_under_test = funit.parser._Scanner(self._source, self._target,
                                                parameters)
        self.assertIsNone(unit_under_test.take_previous_state())
        unit_under_test.give_previous_state(self._DummyState())
        self.assertIsInstance(unit_under_test.take_previous_state(),
                              self._DummyState)
        self.assertIsNone(unit_under_test.take_previous_state())


class TestModuleMatch(unittest.TestCase):
    def test_source(self):
        source = io.StringIO('module boo_mod\n')
        target = io.StringIO()
        unit_under_test = funit.parser.Processor()
        unit_under_test.run(source, target)
        self.assertEqual('boo_mod',
                         unit_under_test.parameters.user_module_name)
        self.assertEqual('Wrapboo_mod',
                         unit_under_test.parameters.wrap_user_module_name)


class _CheckBase(object):
    metaclass = ABCMeta

    @abstractmethod
    def situations(self):
        raise NotImplementedError('Attempt to call an abstract method')

    @abstractmethod
    def check_unit(self, expectation, unit_under_test):
        raise NotImplementedError('Attempt to call an abstract method')

    def test_directive(self):
        unit_under_test = funit.parser.Processor()
        for situation in self.situations():
            with self.subTest(situation=situation):
                source = io.StringIO(situation['source'])
                source.name = 'source_file'
                target = io.StringIO()
                if 'throw' in situation:
                    with self.assertRaises(funit.parser.FUnitException) as ex:
                        unit_under_test.run(source, target)
                    self.assertEqual(situation['throw'], str(ex.exception))
                else:
                    unit_under_test.run(source, target)
                    self.assertEqual(situation['target'], target.getvalue())
                    self.check_unit(situation['expect'],
                                    unit_under_test.parameters)


class _CheckMethod(_CheckBase):
    metaclass = ABCMeta

    def check_unit(self, expectation, parameters):
        self.assertListEqual(expectation, parameters.user_test_methods)


class _CheckCase(_CheckBase):
    metaclass = ABCMeta

    def check_unit(self, expectation, parameters):
        self.assertDictEqual(expectation, parameters.user_test_cases)


class TestAtBefore(unittest.TestCase, _CheckCase):
    def situations(self):
        return [{'source': '  @before\nsubroutine pontefract()\n',
                 'target': '  !@before\nsubroutine pontefract()\n',
                 'expect': {'setUp': {'name': 'pontefract', 'arguments':[]}}},
                {'source': '  @beforeb\nsubroutine clitheroe\n',
                 'throw': 'Unrecognised directive: beforeb'}]


class TestAtAfter(unittest.TestCase, _CheckCase):
    def situations(self):
        return [{'source': '  @after\nsubroutine smethwick()\n',
                 'target': '  !@after\nsubroutine smethwick()\n',
                 'expect': {'tearDown': {'name': 'smethwick', 'arguments':[]}}},
                {'source': '  @afterb\nsubroutine clitheroe\n',
                 'throw': 'Unrecognised directive: afterb'}]


class TestAtTest(unittest.TestCase, _CheckMethod):
    def situations(self):
        return [{'source': '@test\nsubroutine foo()\n',
                 'target': '!@test\nsubroutine foo()\n',
                 'expect': [{'name': 'foo', 'arguments': []}]},
                {'source': '! @test\nsubroutine bar()\n',
                 'target': '! @test\nsubroutine bar()\n',
                 'expect': []},
                # Leading space
                {'source': '  @test\nsubroutine baz()\n',
                 'target': '  !@test\nsubroutine baz()\n',
                 'expect': [{'name': 'baz', 'arguments': []}]},
                # Case insensitive
                {'source': '@TEST\nsubroutine qux()\n',
                 'target': '!@TEST\nsubroutine qux()\n',
                 'expect': [{'name': 'qux', 'arguments': []}]},
                # Mixed case
                {'source': '@Test\nsubroutine fred()\n',
                 'target': '!@Test\nsubroutine fred()\n',
                 'expect': [{'name': 'fred', 'arguments': []}]},
                # Can't have trailing characters without whitespace
                {'source': '@Testb\nsubroutine jim()\n',
                 'throw': 'Unrecognised directive: Testb'},
                {'source': '@Test (b)\nsubroutine sheela()\n',
                 'target': '!@Test (b)\nsubroutine sheela()\n',
                 'expect': [{'name': 'sheela', 'arguments': []}]},
                {'source': '@Test(b)\nsubroutine bob()\n',
                 'target': '!@Test(b)\nsubroutine bob()\n',
                 'expect': [{'name': 'bob', 'arguments': []}]},
                # Next non-space character must be open parenthesis
                {'source': '@Test b\nsubroutine sheela()\n',
                 'throw': 'Malformed @test directive: @Test b'},
                {'source': '@test\nsubroutine warp(this)\n',
                 'target': '!@test\nsubroutine warp(this)\n',
                 'expect': [{'name': 'warp', 'arguments': ['this']}]},
                {'source': '@Test(timeout=3.0)\nsubroutine cheese()\n',
                 'target': '!@Test(timeout=3.0)\nsubroutine cheese()\n',
                 'expect': [{'name': 'cheese', 'arguments': []}]},
                {'source': '@test(cases = [1,3,5])\nsubroutine beef()\n',
                 'target': '!@test(cases = [1,3,5])\nsubroutine beef()\n',
                 'expect': [{'name': 'beef', 'cases': [1, 3, 5],
                             'arguments': []}]},
                {'source': '@test(npes = [7, 11])\nsubroutine teapot()\n',
                 'target': '!@test(npes = [7, 11])\nsubroutine teapot()\n',
                 'expect': [{'name': 'teapot', 'npRequests': [7, 11],
                             'arguments': []}]},
                {'source': '@test(ifdef=BOO)\nsubroutine nebuchadnezzar()\n',
                 'target': '!@test(ifdef=BOO)\nsubroutine nebuchadnezzar()\n',
                 'expect': [{'name': 'nebuchadnezzar', 'ifdef': 'BOO',
                             'arguments': []}]},
                {'source': '@test(ifndef=HOO)\nsubroutine panjandrum()\n',
                 'target': '!@test(ifndef=HOO)\nsubroutine panjandrum()\n',
                 'expect': [{'name': 'panjandrum', 'ifndef': 'HOO',
                             'arguments': []}]},
                {'source': '@test(type=float)\nsubroutine pukka()\n',
                 'target': '!@test(type=float)\nsubroutine pukka()\n',
                 'expect': [{'name': 'pukka', 'type': 'float',
                             'arguments': []}]},
                {'source': '@test(testParameters={function()})\nsubroutine wallah()\n',
                 'target': '!@test(testParameters={function()})\nsubroutine wallah()\n',
                 'expect': [{'name': 'wallah', 'testParameters': 'function()',
                             'arguments': []}]},
                {'source': '@test\nsubroutine first()\n'
                           + '@test\nsubroutine second()\n',
                 'target': '!@test\nsubroutine first()\n'
                           + '!@test\nsubroutine second()\n',
                 'expect': [{'name': 'first', 'arguments': []},
                            {'name': 'second', 'arguments': []}]},
                {'source': '@test\n\nsubroutine one_line()\n',
                 'target': '!@test\n\nsubroutine one_line()\n',
                 'expect': [{'name': 'one_line', 'arguments': []}]},
                {'source': '''@test
! Interposing comment
subroutine one_comment()
''',
                 'target': '''!@test
! Interposing comment
subroutine one_comment()
''',
                 'expect': [{'name': 'one_comment', 'arguments': []}]}]


class TestAtMpiTest(unittest.TestCase, _CheckMethod):
    '''
    Check that a line starting with '@mpitest' is detected as an
    annotation and that optional parameters are collected.
    '''
    def situations(self):
        return [{'source': '@mpitest(npes=[1])\nsubroutine fred()\n',
                 'target': '!@mpitest(npes=[1])\nsubroutine fred()\n',
                 'expect': [{'name': 'fred', 'npRequests': [1],
                                'arguments': []}]},
                {'source': '@mpitest( npes=[1])\nsubroutine wilma()\n',
                 'target': '!@mpitest( npes=[1])\nsubroutine wilma()\n',
                 'expect': [{'name': 'wilma', 'npRequests': [1],
                                'arguments': []}]},
                {'source': '@mpitest(npes=[1, 2,3], ifdef=USE_MPI)\nsubroutine barney()\n',
                 'target': '!@mpitest(npes=[1, 2,3], ifdef=USE_MPI)\nsubroutine barney()\n',
                 'expect': [{'name': 'barney', 'npRequests': [1, 2, 3],
                                'ifdef': 'USE_MPI', 'arguments': []}]},
                {'source': '@mpitest(npes=[3],ifdef=USE_MPI)\nsubroutine betty()\n',
                 'target': '!@mpitest(npes=[3],ifdef=USE_MPI)\nsubroutine betty()\n',
                 'expect': [{'name': 'betty', 'npRequests': [3],
                             'ifdef': 'USE_MPI', 'arguments': []}]}]


class TestTestCase(unittest.TestCase, _CheckCase):
    '''
    Check that a line starting with '@testcase' is detected as an
    annotation.
    '''
    def situations(self):
        return [{'source': '@testcase\ntype hera\n',
                 'target': '!@testcase\ntype hera\n',
                 'expect': {'hera': {'name': 'hera', 'modifiers': []}}},
                # Leading space
                {'source': '  @testcase\ntype zeus\n',
                 'target': '  !@testcase\ntype zeus\n',
                 'expect': {'zeus': {'name': 'zeus', 'modifiers': []}}},
                # Case insensitive
                {'source': '@TESTCASE\ntype metis\n',
                 'target': '!@TESTCASE\ntype metis\n',
                 'expect': {'metis': {'name': 'metis', 'modifiers': []}}},
                # Mixed case
                {'source': '@Testcase\ntype themis\n',
                 'target': '!@Testcase\ntype themis\n',
                 'expect': {'themis': {'name': 'themis', 'modifiers': []}}},
                # Can't have trailing characters without whitespace
                {'source': '@Testcaseb\ntype eurynome\n',
                 'throw': 'Unrecognised directive: Testcaseb'},
                {'source': '@testCase\ntype mnemosyne\n',
                 'target': '!@testCase\ntype mnemosyne\n',
                 'expect': {'mnemosyne': {'name': 'mnemosyne',
                                          'modifiers': []}}},
                {'source': '@testcase\ntype, public :: leto\n',
                 'target': '!@testcase\ntype, public :: leto\n',
                 'expect': {'leto': {'name': 'leto',
                                     'modifiers': ['public']}}},
                {'source': '@testcase\ntype :: nymphs\n@testcase\ntype mortals\n',
                 'target': '!@testcase\ntype :: nymphs\n!@testcase\ntype mortals\n',
                 'expect': {'nymphs': {'name': 'nymphs', 'modifiers': []},
                            'mortals': {'name': 'mortals', 'modifiers': []}}},
                {'source': '@testcase(constructor=build())\ntype artemis\n',
                 'target': '!@testcase(constructor=build())\ntype artemis\n',
                 'expect': {'artemis': {'name': 'artemis',
                                        'constructor': 'build',
                                        'modifiers': []}}},
                {'source': '@testcase(npes=[1, 3,7])\ntype minerva\n',
                 'target': '!@testcase(npes=[1, 3,7])\ntype minerva\n',
                 'expect': {'minerva': {'name': 'minerva',
                                        'npes': [1, 3, 7],
                                        'modifiers': []}}},
                {'source': '''@testcase(cases=[2,4, 6])
type hephaestus
''',
                 'target': '''!@testcase(cases=[2,4, 6])
type hephaestus
''',
                 'expect': {'hephaestus': {'name': 'hephaestus',
                                           'cases': [2, 4, 6],
                                           'modifiers': []}}},
                {'source': '''@testcase(testParameters={enforcer})
type nemesis
''',
                 'target': '''!@testcase(testParameters={enforcer})
type nemesis
''',
                 'expect': {'nemesis': {'name': 'nemesis',
                                        'testParameters': 'enforcer',
                                        'modifiers': []}}}]


class TestSuite(unittest.TestCase, _CheckBase):
    '''
    Check that a line starting with '@suite changes the suite name.
    '''
    def situations(self):
        return [{'source': "  @suite (name='a')\n",
                 'target': "  !@suite (name='a')\n",
                 'expect': {'name': 'a', 'wrapper': 'Wrapa'}},
                {'source': '  @suite (name="b")\n',
                 'target': '  !@suite (name="b")\n',
                 'expect': {'name':'b', 'wrapper':'Wrapb'}},
                {'source': "  @suite(name='aa')\n",
                 'target': "  !@suite(name='aa')\n",
                 'expect': {'name':'aa', 'wrapper':'Wrapaa'}},
                {'source': '  @suite(name=a b)\n',
                 'throw': '@suite directive must provide a name'},
                {'source': '  @suiteb()\n',
                 'throw': 'Unrecognised directive: suiteb'},
                {'source': '  @suite()\n',
                 'throw': '@suite directive must provide a name'}]

    def check_unit(self, expectation, parameters):
        self.assertEqual(expectation['name'], parameters.suite_name)
        self.assertEqual(expectation['wrapper'],
                         parameters.wrap_user_module_name)


class TestAssertEqual(unittest.TestCase, _CheckBase):
    '''
    Check that a line starting with '@assertEqual' is detected as an
    annotation.
    '''
    def situations(self):
        return [{'source': '@assertEqual\n',
                 'throw': 'Mangled assert directive: @assertEqual'},
                {'source': '@assertEqual()\n',
                 'throw': 'Mangled assert directive: @assertEqual()'},
                {'source': '@assertEqual(a, b)\n',
                 'target': '''!@assertEqual(a, b)
#line 1 "source_file"
  call assertEqual(a, b, &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                # Case insensitive
                {'source': '@assertequal(a, b)\n',
                 'target': '''!@assertequal(a, b)
#line 1 "source_file"
  call assertEqual(a, b, &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                # Case insensitive
                {'source': '@ASSERTEQUAL(a, b)\n',
                 'target': '''!@ASSERTEQUAL(a, b)
#line 1 "source_file"
  call assertEqual(a, b, &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''}]

    def check_unit(self, expectation, parameters):
        pass


class TestAssertAssociated(unittest.TestCase, _CheckBase):
    '''
    Check that a line starting with '@assertAssociated' is detectd as an
    annotiation. This annotation accepts 1 or 2 arguments. A single argument
    tests for simple association. Two arguments means that the first must be
    associated with the second.
    '''
    def situations(self):
        return [{'source': '@assertAssociated\n',
                 'throw': 'Mangled assertAssociated directive: @assertAssociated'},
                {'source': '@assertAssociated()\n',
                 'throw': 'Mangled assertAssociated directive: @assertAssociated()'},
                {'source': '@assertAssociated(a)\n',
                 'target': '''!@assertAssociated(a)
#line 1 "source_file"
  call assertTrue(associated(a), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                # Case insensitive
                {'source': '@assertassociated(a)\n',
                 'target': '''!@assertassociated(a)
#line 1 "source_file"
  call assertTrue(associated(a), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                 # Case insensitive
                {'source': '@ASSERTASSOCIATED(a)\n',
                 'target': '''!@ASSERTASSOCIATED(a)
#line 1 "source_file"
  call assertTrue(associated(a), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                 {'source': '@assertAssociated(a, message="Filthy bobbins!")\n',
                  'target': '''!@assertAssociated(a, message="Filthy bobbins!")
#line 1 "source_file"
  call assertTrue(associated(a), &
    message='Filthy bobbins!', &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                  'expect': ''},
                 {'source': '@assertAssociated(a,b)\n',
                  'target': '''!@assertAssociated(a,b)
#line 1 "source_file"
  call assertTrue(associated(a, b), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                  'expect': ''},
                # Case insensitive
                {'source': '@assertassociated(a,b)\n',
                 'target': '''!@assertassociated(a,b)
#line 1 "source_file"
  call assertTrue(associated(a, b), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                 # Case insensitive
                {'source': '@ASSERTASSOCIATED(a,b)\n',
                 'target': '''!@ASSERTASSOCIATED(a,b)
#line 1 "source_file"
  call assertTrue(associated(a, b), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                 {'source': '@assertAssociated(a, b, message="Turgid shuttles!")\n',
                  'target': '''!@assertAssociated(a, b, message="Turgid shuttles!")
#line 1 "source_file"
  call assertTrue(associated(a, b), &
    message='Turgid shuttles!', &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                  'expect': ''}]

    def check_unit(self, expectation, parameters):
        pass

class TestAssertNotAssociated(unittest.TestCase, _CheckBase):
    '''
    Check that a line starting with '@assertUnAssociated' is detectd as an
    annotiation. This annotation accepts 1 or 2 arguments. A single argument
    tests for simple association. Two arguments means that the first must be
    associated with the second.
    '''
    def situations(self):
        return [{'source': '@assertUnAssociated\n',
                 'throw': 'Mangled assertNotAssociated directive: @assertUnAssociated'},
                {'source': '@assertNotAssociated\n',
                 'throw': 'Mangled assertNotAssociated directive: @assertNotAssociated'},
                {'source': '@assertUnAssociated()\n',
                 'throw': 'Mangled assertNotAssociated directive: @assertUnAssociated()'},
                {'source': '@assertNotAssociated()\n',
                 'throw': 'Mangled assertNotAssociated directive: @assertNotAssociated()'},
                {'source': '@assertUnAssociated(a)\n',
                 'target': '''!@assertUnAssociated(a)
#line 1 "source_file"
  call assertFalse(associated(a), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                {'source': '@assertNotAssociated(a)\n',
                 'target': '''!@assertNotAssociated(a)
#line 1 "source_file"
  call assertFalse(associated(a), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                # Case insensitive
                {'source': '@assertunassociated(a)\n',
                 'target': '''!@assertunassociated(a)
#line 1 "source_file"
  call assertFalse(associated(a), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                # Case insensitive
                {'source': '@ASSERTUNASSOCIATED(a)\n',
                 'target': '''!@ASSERTUNASSOCIATED(a)
#line 1 "source_file"
  call assertFalse(associated(a), &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''}]

    def check_unit(self, expectation, parameters):
        pass


class TestAssertEquivalent(unittest.TestCase, _CheckBase):
    '''
    Check that a line starting with '@assertAssociated' is detectd as an
    annotiation. This annotation accepts 1 or 2 arguments. A single argument
    tests for simple association. Two arguments means that the first must be
    associated with the second.
    '''
    def situations(self):
        return [{'source': '@assertEquivalent\n',
                 'throw': 'Mangled assertEquivalent directive: @assertEquivalent'},
                {'source': '@assertEquivalent()\n',
                 'throw': 'Mangled assertEquivalent directive: @assertEquivalent()'},
                {'source': '@assertEquivalent(a)\n',
                 'throw': 'Mangled assertEquivalent directive: @assertEquivalent(a)'},
                # Case insensitive
                {'source': '@assertequivalent(a, b)\n',
                 'target': '''!@assertequivalent(a, b)
#line 1 "source_file"
  call assertTrue(a .eqv. b, &
    message='<a> not equal to <b>', &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''},
                # Case insensitive
                {'source': '@ASSERTEQUIVALENT(a, b)\n',
                 'target': '''!@ASSERTEQUIVALENT(a, b)
#line 1 "source_file"
  call assertTrue(a .eqv. b, &
    message='<a> not equal to <b>', &
    location=SourceLocation( &
      'source_file', &
      1)
    )
  if (anyExceptions()) return
#line 2 "source_file"
''',
                 'expect': ''}]

    def check_unit(self, expectation, parameters):
        pass


#class TestParseLine(unittest.TestCase):

    #def testCppSetLineAndFile(self):
        #self.assertEqual('#line 7 "foo"\n', cppSetLineAndFile(7, 'foo'))
        #self.assertEqual('#line 3 "bar"\n', cppSetLineAndFile(3, 'bar'))

    #def testGetSubroutineName(self):
        #self.assertEqual('a', getSubroutineName('subroutine a()'))
        #self.assertEqual('abcd', getSubroutineName('subroutine   abcd ()'))

    #def testGetSelfObjectName(self):
        #self.assertEqual('b', getSelfObjectName('subroutine a(b)'))
        #self.assertEqual('bc', getSelfObjectName('subroutine a(bc)'))
        #self.assertEqual('bc', getSelfObjectName('subroutine a(bc,d)'))
        #self.assertEqual('bc', getSelfObjectName('subroutine a(bc, d)'))
        #self.assertEqual('bc', getSelfObjectName('subroutine a(bc ,d)'))
        #self.assertEqual('bc', getSelfObjectName('subroutine a(bc , d)'))


    #def testGetTypeName(self):
        #self.assertEqual('foo', getTypeName(' type :: foo'))
        #self.assertEqual('foo', getTypeName(' type, extends(something) :: foo'))
        #self.assertEqual('foo', getTypeName(' type, abstract :: foo'))
        #self.assertEqual('foo', getTypeName(' type, extends(something), abstract :: foo'))


    #def testAtTestNoParens(self):
        #"""Check that test procedure with no parens is accepted."""
        #nextLine = 'subroutine myTest ! and a comment \n'
        #parser = MockParser([nextLine])
        #atTest = AtTest(parser)

        #m = atTest.match('@test\n')
        #atTest.action(m,'@test\n')
        #self.assertEqual('myTest', parser.userTestMethods[0]['name'])
        #self.assertEqual('!@test\n',parser.outLines[0])
        #self.assertEqual(nextLine,parser.outLines[1])

    #def testAtTestFail(self):
        #"""Check that useful error is sent if next line is not properly formatted."""

        #nextLine = 'subroutine myTest (] \n' # bad closing paren
        #parser = MockParser([nextLine])
        
        #with self.assertRaises(MyError):
            #atTest = AtTest(parser)
            #line = '@test'
            #m = atTest.match(line)
            #atTest.action(m, line)


    #def testAtTestSkipComment(self):
        #"""Ignore comment lines between @test and subroutine foo()."""
        #nextLineA = '! ignore this line \n'
        #nextLineB = '\n'
        #nextLineC = 'subroutine myTestC()\n'
        #parser = MockParser([nextLineA,nextLineB,nextLineC])

        #atTest = AtTest(parser)
        #atTest.apply('@test\n')
        #self.assertEqual('myTestC', parser.userTestMethods[0]['name'])
        #self.assertEqual('!@test\n',parser.outLines[0])
        #self.assertEqual(nextLineC,parser.outLines[1])

        




    #def testParseArgsFirstRest(self):
        #"""Test that the first-rest argument parsing is adequate."""
        #self.assertEqual(['a1','b1'],parseArgsFirstRest('','a1,b1'))
        #self.assertEqual(['a4()','b4'],parseArgsFirstRest('','a4(),b4'))
        #self.assertEqual(['a4%z()','b4'],parseArgsFirstRest('','a4%z(),b4'))
        #self.assertEqual(['a4','b4%z()'],parseArgsFirstRest('','a4,b4%z()'))
        #self.assertEqual(['a10','b10,c10'],parseArgsFirstRest('','a10,b10,c10'))
        #self.assertEqual(['a2'],parseArgsFirstRest("@assertassociated","@assertassociated(a2)"))
        #self.assertEqual(['a3',"b3,message='This is the message.'"], \
            #parseArgsFirstRest("@assertassociated","@assertassociated(a3,b3,message='This is the message.')"))
        #self.assertEqual(['a','b,c,d'],parseArgsFirstRest("@assertassociated","@assertassociated(a,b,c,d)"))

    #def testParseArgsFirstSecondRest(self):
        #"""Test that the first-second-rest argument parsing is adequate."""
        #self.assertEqual(None,parseArgsFirstSecondRest("@assertassociated","@assertassociated"))
        #self.assertEqual(None,parseArgsFirstSecondRest("@assertassociated","@assertassociated()"))
        #self.assertEqual(['a'],parseArgsFirstSecondRest("@assertassociated","@assertassociated(a)"))
        #self.assertEqual(['a','b'],parseArgsFirstSecondRest("@assertassociated","@assertassociated(a,b)"))
        #self.assertEqual(['a','b',"message='This is the message.'"], \
            #parseArgsFirstSecondRest("@assertassociated", \
                                     #"@assertassociated(a,b,message='This is the message.')"))
        #self.assertEqual(['a','b%z()','c,d'],parseArgsFirstSecondRest("@assertassociated", \
                                                                  #"@assertassociated(a,b%z(),c,d)"))
        #self.assertEqual(['a4','b4','c4'],parseArgsFirstSecondRest('','a4,b4,c4'))
                                                                  





    #def testMatchAtAssertUnAssociatedWith(self):
        #"""Check that a line starting with '@assertUnAssociatedWith' is detected
        #as an annotation. atAssertUnAssociated(a,b) implies a points to b."""
        #parser = MockParser([' \n'])
        #atAssertUnAssociated = AtAssertNotAssociated(parser)

        #self.assertFalse(atAssertUnAssociated.match('@assertUnAssociated'))
        #self.assertFalse(atAssertUnAssociated.match('@assertUnAssociated()'))
        #self.assertTrue(atAssertUnAssociated.match('@assertUnAssociated(a)'))
        #self.assertTrue(atAssertUnAssociated.match('@assertunassociated(a,b)')) # case insensitive
        #self.assertTrue(atAssertUnAssociated.match('@ASSERTUNASSOCIATED(a,b)')) # case insensitive

        #parser.fileName = "foo.pfunit"
        #parser.currentLineNumber = 8
        #atAssertUnAssociated.apply('   @assertUnAssociated(a,b)\n')
        #self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        #self.assertEqual("  call assertFalse(associated(a,b), &\n", parser.outLines[1])
        #self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        #self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        #self.assertEqual(" & 8)", parser.outLines[4])
        #self.assertEqual(" )\n", parser.outLines[5])
        #self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        #self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

    #def testMatchAtAssertNotassociated(self):
        #"""Check that a line starting with '@assertNotAssociated' is detected
        #as an annotation."""
        #parser = MockParser([' \n'])
        #atAssertNotassociated = AtAssertNotAssociated(parser)

        #self.assertFalse(atAssertNotassociated.match('@assertNotassociated'))
        #self.assertFalse(atAssertNotassociated.match('@assertNotassociated()'))
        #self.assertTrue(atAssertNotassociated.match('@assertNotassociated(a)'))
        #self.assertTrue(atAssertNotassociated.match('@assertnotassociated(a)')) # case insensitive
        #self.assertTrue(atAssertNotassociated.match('@ASSERTNOTASSOCIATED(a)')) # case insensitive

        #parser.fileName = "foo.pfunit"
        #parser.currentLineNumber = 8
        #atAssertNotassociated.apply('   @assertNotassociated(a)\n')
        #self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        #self.assertEqual("  call assertFalse(associated(a), &\n", parser.outLines[1])
        #self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        #self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        #self.assertEqual(" & 8)", parser.outLines[4])
        #self.assertEqual(" )\n", parser.outLines[5])
        #self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        #self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])

        

    #def testMatchAtAssertEqualUserDefined(self):
        #"""Check that a line starting with '@assertEqualUserDefined' is detected
        #as an annotation. atAssertEqualUserDefined(a,b) implies a points to b."""
        #parser = MockParser([' \n'])
        #atAssertEqualUserDefined = AtAssertEqualUserDefined(parser)

        #self.assertFalse(atAssertEqualUserDefined.match('@assertEqualUserDefined'))
        #self.assertFalse(atAssertEqualUserDefined.match('@assertEqualUserDefined()'))
        #self.assertFalse(atAssertEqualUserDefined.match('@assertEqualUserDefined(a)'))
        #self.assertTrue(atAssertEqualUserDefined.match('@assertequaluserdefined(a,b)')) # case insensitive
        #self.assertTrue(atAssertEqualUserDefined.match('@ASSERTEQUALUSERDEFINED(a,b)')) # case insensitive

        #parser.fileName = "foo.pfunit"
        #parser.currentLineNumber = 8
        #atAssertEqualUserDefined.apply('   @assertEqualUserDefined(a,b)\n')
        #self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        #self.assertEqual("  call assertTrue(a==b, &\n", parser.outLines[1])
        #self.assertEqual(" & message='<a> not equal to <b>', &\n", parser.outLines[2])
        #self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[3])
        #self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[4])
        #self.assertEqual(" & 8)", parser.outLines[5])
        #self.assertEqual(" )\n", parser.outLines[6])
        #self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[7])
        #self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[8])

    #def testMatchAtAssertEqualUserDefinedWithMessage(self):
        #"""Check that a line starting with '@assertEqualUserDefined' is detected
        #as an annotation. atAssertEqualUserDefined(a,b) implies a points to b."""
        #parser = MockParser([' \n'])
        #atAssertEqualUserDefined = AtAssertEqualUserDefined(parser)

        #parser.fileName = "foo.pfunit"
        #parser.currentLineNumber = 8
        #atAssertEqualUserDefined.apply('   @assertEqualUserDefined(a,b,message="c")\n')
        #self.assertEqual('#line 8 "foo.pfunit"\n', parser.outLines[0])
        #self.assertEqual('  call assertTrue(a==b, message="c", &\n', parser.outLines[1])
        #self.assertEqual(" & location=SourceLocation( &\n", parser.outLines[2])
        #self.assertEqual(" & 'foo.pfunit', &\n", parser.outLines[3])
        #self.assertEqual(" & 8)", parser.outLines[4])
        #self.assertEqual(" )\n", parser.outLines[5])
        #self.assertEqual("  if (anyExceptions()) return\n", parser.outLines[6])
        #self.assertEqual('#line 9 "foo.pfunit"\n', parser.outLines[7])



        
    #def testMatchAtAssertOther(self):
        #"""Check that a line starting with '@assert*' is detected
        #as an annotation."""
        #parser = MockParser([' \n'])
        #atAssert = AtAssert(parser)

        #self.assertFalse(atAssert.match('@assertTrue'))
        #self.assertFalse(atAssert.match('@assertTrue()'))
        #self.assertTrue(atAssert.match('@assertTrue(a)'))
        #self.assertTrue(atAssert.match('@asserttrue(a)')) # case insensitive
        #self.assertTrue(atAssert.match('@ASSERTTRUE(a)')) # case insensitive

        #parser.fileName = 'foo.pfunit'
        #parser.currentLineNumber = 8
        #atAssert.apply('   @assertTrue(.true.)\n')
        #self.assertTrue("#line 8 'foo.pfunit'\n", parser.outLines[0])
        #self.assertTrue("  call assertTrue(1, 2, &\n", parser.outLines[1])
        #self.assertTrue(" & location=SourceLocation( &\n", parser.outLines[2])
        #self.assertTrue(" & 'foo.pfunit', &\n", parser.outLines[3])
        #self.assertTrue(" & 8)", parser.outLines[4])
        #self.assertTrue(" )\n", parser.outLines[5])
        #self.assertTrue("  if (anyExceptions()) return\n", parser.outLines[6])
        #self.assertTrue("#line 9 'foo.pfunit'\n", parser.outLines[7])

    #def testMatchAtMpiAssert(self):
        #"""Check that a line starting with '@mpiAssert*' is detected
        #as an annotation."""
        #parser = MockParser(['subroutine foo(this)\n'])
        #atMpiAssert = AtMpiAssert(parser)

        #self.assertFalse(atMpiAssert.match('@mpiAssertTrue'))
        #self.assertFalse(atMpiAssert.match('@mpiAssertTrue()'))
        #self.assertTrue(atMpiAssert.match('@mpiAssertTrue(a)'))
        #self.assertTrue(atMpiAssert.match('@mpiAssertTrue(a,b)'))
        #self.assertTrue(atMpiAssert.match('@mpiasserttrue(a)')) # case insensitive
        #self.assertTrue(atMpiAssert.match('@MPIASSERTTRUE(a)')) # case insensitive

        #parser.fileName = 'foo.pfunit'
        #parser.currentLineNumber = 8
        #atMpiAssert.apply('   @mpiAssertTrue(.true.)\n')
        #self.assertTrue("#line 8 'foo.pfunit'\n", parser.outLines[0])
        #self.assertTrue("  call assertTrue(1, 2, &\n", parser.outLines[1])
        #self.assertTrue(" & location=SourceLocation( &\n", parser.outLines[2])
        #self.assertTrue(" & 'foo.pfunit', &\n", parser.outLines[3])
        #self.assertTrue(" & 8)", parser.outLines[4])
        #self.assertTrue(" )\n", parser.outLines[5])
        #self.assertTrue("  if (anyExceptions(this%getMpiCommunicator())) return\n", parser.outLines[6])
        #self.assertTrue("#line 9 'foo.pfunit'\n", parser.outLines[7])

if __name__ == "__main__":
    unittest.main()
