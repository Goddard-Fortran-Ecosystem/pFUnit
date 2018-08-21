#!/usr/bin/env python

from abc import ABCMeta, abstractmethod
import os.path
import re


class FUnitException(Exception):
    pass


class _State(object):
    metaclass = ABCMeta

    @abstractmethod
    def scan(self, scanner):
        raise NotImplementedError('Attempted to call an abstract method')


class _StateReturn(_State):
    def scan(self, scanner):
        raise NotImplementedError('Attempted to scan a return state')


class _CodeMatcher(_State):
    def scan(self, scanner):
        line = scanner.take_line()
        match = self.pattern().match(line)
        if match:
            self.action(match, scanner)
        else:
            scanner.give_line(line)


class _ModuleMatcher(_CodeMatcher):
    _CODE_PATTERN = re.compile(r'^\s*module\s+(\w+)\s*$', re.IGNORECASE)

    @classmethod
    def pattern(cls):
        return cls._CODE_PATTERN

    @classmethod
    def action(cls, match, scanner):
        scanner.set_user_module_name(match.group(1))


class _SubroutineState(_State):
    _SUBROUTINE_PATTERN = re.compile(r'^\s*subroutine\s+(\w+)\s*\(([\w\s,]+)?\)', re.IGNORECASE)

    def __init__(self):
        self.name = None
        self.arguments = None

    def scan(self, scanner):
        line = scanner.take_line()
        scanner.give_line(line)
        match = self._SUBROUTINE_PATTERN.match(line)
        if match:
            self.name = match.group(1)
            if match.group(2):
                self.arguments = match.group(2).split(',')
            else:
                self.arguments = []
            return _StateReturn()
        else:
            raise FUnitException('Expected subroutine definition, found: '
                                 + line)


class _TypeState(_State):
    _TYPE_PATTERN = re.compile(r'^\s*type(?:\s*|,([\w\s,]+))(?:\s*::\s*)?(\w+)\s*$',
                               re.IGNORECASE)

    def __init__(self):
        self.name = None
        self.modifiers = None

    def scan(self, scanner):
        line = scanner.take_line()
        scanner.give_line(line)
        match = self._TYPE_PATTERN.match(line)
        if match:
            self.name = match.group(2)
            if match.group(1):
                self.modifiers = [mod.strip()
                                  for mod in match.group(1).split(',')]
            else:
                self.modifiers = []
            return _StateReturn()
        else:
            raise FUnitException('Expected type definition, found: ' + line)


class _StateDirective(_State):
    def __init__(self):
        self._details = {}

    @abstractmethod
    def directive(self):
        pass

    @abstractmethod
    def options(self):
        pass

    def scan(self, scanner):
        directive_tag, directive_pattern = self.directive()
        line = scanner.take_line()
        directive_match = directive_pattern.match(line)
        if directive_match:
            argument_string = directive_match.group(1)
            if argument_string:
                for key, (pattern, value, index) in self.options().items():
                    option_match = pattern.search(argument_string)
                    if option_match:
                        self._details[key] = value(option_match, index)
        else:
            raise FUnitException('Malformed @{} directive: {}' \
                                 .format(directive_tag, line.strip()))


_INT_LIST_LAMBDA = lambda match, group: \
    list(map(int, match.group(group).split(',')))
_STR_LAMBDA = lambda match, group: match.group(group)


class _TestStateBase(_StateDirective):
    _NPES_PATTERN = re.compile(r'npes\s*=\s*\[([0-9,\s]+)\]', re.IGNORECASE)
    _IFDEF_PATTERN = re.compile(r'ifdef\s*=\s*(\w+)', re.IGNORECASE)
    _IFNDEF_PATTERN = re.compile(r'ifndef\s*=\s*(\w+)', re.IGNORECASE)
    _TYPE_PATTERN = re.compile(r'type\s*=\s*(\w+)', re.IGNORECASE)
    _CASES_PATTERN = re.compile(r'cases\s*=\s*\[([0-9,\s]+)\]', re.IGNORECASE)
    _PARAMETERS_PATTERN = re.compile(r'testParameters\s*=\s*[{](.*)[}]',
                                     re.IGNORECASE)

    _OPTIONS = {'npRequests': (_NPES_PATTERN, _INT_LIST_LAMBDA, 1),
                'ifdef': (_IFDEF_PATTERN, _STR_LAMBDA, 1),
                'ifndef': (_IFNDEF_PATTERN, _STR_LAMBDA, 1),
                'type': (_TYPE_PATTERN, _STR_LAMBDA, 1),
                'cases': (_CASES_PATTERN, _INT_LIST_LAMBDA, 1),
                'testParameters': (_PARAMETERS_PATTERN, _STR_LAMBDA, 1)}

    def options(self):
        return self._OPTIONS

    def scan(self, scanner):
        directive_tag, directive_pattern = self.directive()
        previous_state = scanner.take_previous_state()
        if previous_state:
            if not isinstance(previous_state, _SubroutineState):
                text = 'Expected a subroutine to follow @{} but found: {}'
                raise FUnitException(text.format(directive_tag,
                    previous_state.__class__.__name__))
            self._details['name'] = previous_state.name
            self._details['arguments'] = previous_state.arguments
            scanner.add_user_test_method(self._details)
            self._details= {}
            return _StateReturn()

        super().scan(scanner)

        return _SubroutineState()


class _TestState(_TestStateBase):
    _DIRECTIVE_PATTERN = re.compile(r'^\s*@test\s*(?:\((.*)\))?\s*$',
                                    re.IGNORECASE)

    def directive(self):
        return 'test', self._DIRECTIVE_PATTERN



class _MpiTestState(_TestStateBase):
    '''
    Deprecated
    '''
    _DIRECTIVE_PATTERN = re.compile(r'^\s*@mpitest\s*(?:\((.*)\))?\s*$',
                                    re.IGNORECASE)

    def directive(self):
        return 'mpitest', self._DIRECTIVE_PATTERN


class _TestCaseState(_StateDirective):
    _DIRECTIVE_PATTERN = re.compile(r'^\s*@testcase\s*(?:|\((.*)\))\s*$',
                                    re.IGNORECASE)

    _CONSTRUCTOR_PATTERN = re.compile(r'constructor\s*=\s*(\w+)',
                                      re.IGNORECASE)
    _NPES_PATTERN = re.compile(r'npes\s*=\s*\[([0-9,\s]+)\]', re.IGNORECASE)
    _CASES_PATTERN = re.compile(r'cases\s*=\s*\[([0-9,\s]+)\]', re.IGNORECASE)
    _PARAMETERS_PATTERN = re.compile(r'testParameters\s*=\s*[{](.*)[}]',
                                     re.IGNORECASE)

    _OPTIONS = {'constructor': (_CONSTRUCTOR_PATTERN, _STR_LAMBDA, 1),
                'npes': (_NPES_PATTERN, _INT_LIST_LAMBDA, 1),
                'cases': (_CASES_PATTERN, _INT_LIST_LAMBDA, 1),
                'testParameters': (_PARAMETERS_PATTERN, _STR_LAMBDA, 1)}

    def directive(self):
        return 'testcase', self._DIRECTIVE_PATTERN

    def options(self):
        return self._OPTIONS

    def scan(self, scanner):
        previous_state = scanner.take_previous_state()
        if previous_state:
            if not isinstance(previous_state, _TypeState):
                text = 'Expected a type to follow @testcase but found: {}'
                raise FUnitException(text.format(previous_state))
            self._details['name'] = previous_state.name
            self._details['modifiers'] = previous_state.modifiers
            scanner.add_user_test_case(self._details)
            self._details={}
            return _StateReturn()

        super().scan(scanner)

        return _TypeState()


class _SuiteState(_StateDirective):
    _DIRECTIVE_PATTERN = re.compile(r'^\s*@suite\s*\((.*)\)\s*$',
                                    re.IGNORECASE)

    _NAME_PATTERN = re.compile(r'name\s*=\s*(?P<quote>[\'"])(\w+)(?P=quote)',
                               re.IGNORECASE)

    _OPTIONS = {'name': (_NAME_PATTERN, _STR_LAMBDA, 2)}

    def directive(self):
        return 'suite', self._DIRECTIVE_PATTERN

    def options(self):
        return self._OPTIONS

    def scan(self, scanner):
        super().scan(scanner)

        if 'name' not in self._details:
            raise FUnitException('@suite directive must provide a name')
        scanner.set_suite_name(self._details['name'])


assertVariants = ('Fail', 'Equal', 'True', 'False', 'LessThan',
                  'LessThanOrEqual', 'GreaterThan', 'GreaterThanOrEqual',
                  'IsMemberOf', 'Contains', 'Any', 'All', 'NotAll', 'None',
                  'IsPermutationOf', 'ExceptionRaised', 'SameShape', 'IsNaN',
                  'IsFinite')


class _AssertState(_State):
    _DIRECTIVE_PATTERN = re.compile(r'^\s*@assert('
                                    + r'|'.join(assertVariants)
                                    + r')\s*\(\s*(\w+)\s*,\s*(\w+)\s*\)\s*$',
                                    re.IGNORECASE)

    def directive(self):
        return 'assert', self._DIRECTIVE_PATTERN

    def scan(self, scanner):
        line = scanner.take_line()
        directive_match = self._DIRECTIVE_PATTERN.match(line)
        if directive_match:
            scanner.emmit_linemarker()
            scanner.emmit_line('  call assert{}({}, {}, &' \
                               .format(directive_match.group(1).capitalize(),
                                       directive_match.group(2),
                                       directive_match.group(3)))
            scanner.emmit_line('    location=SourceLocation( &')
            scanner.emmit_line('      \'{source_file}\', &')
            scanner.emmit_line('      {source_line_number})')
            scanner.emmit_line('    )')
            scanner.emmit_line('  if (anyExceptions()) return')
            scanner.emmit_linemarker(offset=1)
        else:
            raise FUnitException('Mangled assert directive: ' + line.strip())


class _SeekState(_State):
    _DIRECTIVE_PATTERN = re.compile(r'^\s*(@)(\w+)')

    _DIRECTIVE_MAP = {'mpitest': _MpiTestState,
                      'suite': _SuiteState,
                      'test': _TestState,
                      'testcase': _TestCaseState}
    _LINE_MAP = {'module': _ModuleMatcher}

    def __init__(self, output_file):
        self._output_file = output_file

        self._DIRECTIVE_MAP.update({'assert' + name.lower(): _AssertState
                                    for name in assertVariants})

    def scan(self, scanner):
        previous_state = scanner.take_previous_state()
        line = scanner.take_line()
        match = self._DIRECTIVE_PATTERN.match(line)
        if match:
            at_pos = match.start(1)
            self._output_file.write(line[:at_pos] + '!' + line[at_pos:])
            scanner.give_line(line)

            new_state_class = self._DIRECTIVE_MAP.get(match.group(2).lower())
            if new_state_class is None:
                text = 'Unrecognised directive: ' + match.group(2)
                raise FUnitException(text)
            return new_state_class()
        else:
            for matcher in self._LINE_MAP.values():
                match = matcher.pattern().match(line)
                if match:
                    matcher.action(match, scanner)
                    return
            self._output_file.write(line)


class Parameters(object):
    def __init__(self):
        self.user_test_methods = []
        self.user_test_cases = []
        self.suite_name = None
        self.user_module_name = None
        self.wrap_user_module_name = None


class _Scanner(object):
    def __init__(self, source_file, target_file, parameters,
                 line_directive=True):
        self._parameters = parameters
        self._previous_state = None

        self._local_source_file = isinstance(source_file, str)
        if self._local_source_file:
            self._source_file = open(input_file, 'r')
        else:
            self._source_file = source_file

        self._local_target_file = isinstance(target_file, str)
        if self._local_target_file:
            self._target_file = open(target_file, 'r')
        else:
            self._target_file = target_file

        self._buffer = self._source_file.readlines()
        self._line_number = 0

        if line_directive is True:
            self._line_directive = 'line'
        else:
            self._line_directive = ''

    def __del__(self):
        if self._local_target_file:
            self._close(target_file)

        if self._local_target_file:
            close(self._target_file)

    def done(self):
        return len(self._buffer) == 0

    def give_line(self, line):
        self._buffer.insert(0, line)
        self._line_number -= 1

    def take_line(self):
        try:
            line = self._buffer.pop(0)
            self._line_number += 1
            return line
        except IndexError:
            return None

    def give_previous_state(self, state):
        if self._previous_state is not None:
            raise FUnitException('Attempt to over write previous state')
        else:
            self._previous_state = state

    def take_previous_state(self):
        previous_state = self._previous_state
        self._previous_state = None
        return previous_state

    def emmit_linemarker(self, offset=0):
        print('#{directive} {number} "{filename}"' \
              .format(directive=self._line_directive,
                      number=str(self._line_number + offset),
                      filename=os.path.basename(self._source_file.name)),
              file=self._target_file)

    def emmit_line(self, line):
        source_leafname = os.path.basename(self._source_file.name)
        print(line.format(source_file=source_leafname,
                          source_line_number=self._line_number),
              file=self._target_file)

    def add_user_test_method(self, details):
        self._parameters.user_test_methods.append(details)

    def add_user_test_case(self, details):
        self._parameters.user_test_cases.append(details)

    def set_suite_name(self, name):
        self._parameters.suite_name = name
        self._parameters.wrap_user_module_name = 'Wrap' + name

    def set_user_module_name(self, name):
        self._parameters.user_module_name = name
        self._parameters.wrap_user_module_name = 'Wrap' + name
        if not self._parameters.suite_name:
            self._parameters.suite_name = name + "_suite"


class Processor(object):
    def run(self, input_file, output_file):
        self.parameters = Parameters()
        scanner = _Scanner(input_file, output_file, self.parameters)

        state = [_SeekState(output_file)]
        while not scanner.done():
            new_state = state[-1].scan(scanner)
            if isinstance(new_state, _StateReturn):
                scanner.give_previous_state(state.pop(-1))
            elif new_state is None:
                pass
            else:
                state.append(new_state)


#from os.path import *
#import re
## from parseBrackets import parseBrackets
#from funit.parseDirectiveArgs import parseDirectiveArguments

#class MyError(Exception):
    #def __init__(self, value):
        #self.value = value
    #def __str__(self):
        #return repr(self.value)

#assertVariants = 'Fail|Equal|NotEqual|True|False|LessThan|LessThanOrEqual|GreaterThan|GreaterThanOrEqual'
#assertVariants += '|NotEqual|RelativelyEqual'
#assertVariants += '|IsInfinite|IsFinite|IsNaN'
#assertVariants += '|IsMemberOf|Contains|Any|All|NotAll|None|IsPermutationOf'
#assertVariants += '|ExceptionRaised|SameShape'

#def parseArgsFirstRest(directiveName,line):
    #"""If the @-directive has more than one argument, parse into first and rest strings.
    #Added for assertAssociated.
    #"""

    #argStr = '';
    #if directiveName != '':
        #m = re.match('\s*'+directiveName+'\s*\\((.*\w.*)\\)\s*$',line,re.IGNORECASE)
        #if m:
            #argStr = m.groups()[0]
        #else:
            #return None
    #else:
        #argStr = line

    #args = parseDirectiveArguments(argStr)

    #if args == []:
        #returnArgs = None
    #elif len(args) == 1:
        #returnArgs = [args[0]]
    #else:
        #returnArgs = [args[0],','.join(args[1:])]

    #return returnArgs


#def parseArgsFirstSecondRest(directiveName,line):
    #"""If the @-directive must have at least two arguments, parse into first, second,
    #and rest strings. Added for assertAssociated.
    #"""
    #args1 = parseArgsFirstRest(directiveName,line)

    #returnArgs = None

    #if args1 != None:
        #if len(args1) == 1:
            #returnArgs = args1
        #elif len(args1) == 2:
            #args2 = parseArgsFirstRest('',args1[1])
            #returnArgs = [args1[0]] + args2
        #elif len(args1) == 3:
            #print(-999,'parseArgsFirstSecondRest::error!')
            #returnArgs = None

    #return returnArgs



##------------------









#class AtAssertAssociated(Action):
    #def __init__(self,parser):
        #self.parser = parser

    #def match(self, line):
        #m = re.match('\s*@assertassociated\s*\\((.*\w.*)\\)\s*$', line, re.IGNORECASE)

        #if not m:
            #m  = re.match( \
                #'\s*@assertassociated\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*),(.*\w*.*))\\)\s*$', \
                #line, re.IGNORECASE)

        ## How to get both (a,b) and (a,b,c) to match?
        #if not m:
            #m  = re.match( \
                #'\s*@assertassociated\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*))\\)\s*$', \
                #line, re.IGNORECASE)
        #return m

    #def appendSourceLocation(self, fileHandle, fileName, lineNumber):
        #fileHandle.write(" & location=SourceLocation( &\n")
        #fileHandle.write(" & '" + str(basename(fileName)) + "', &\n")
        #fileHandle.write(" & " + str(lineNumber) + ")")

    #def action(self, m, line):
        #p = self.parser

        ## args = parseArgsFirstRest('@assertassociated',line)
        #args = parseArgsFirstSecondRest('@assertassociated',line)

        ## print(9000,line)
        ## print(9001,args)

        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber, p.fileName))
        #if len(args) > 1:
            #if re.match('.*message=.*',args[1],re.IGNORECASE):
                #p.outputFile.write("  call assertTrue(associated(" + args[0] + "), " + args[1] + ", &\n")
            #elif len(args) > 2:
                #p.outputFile.write("  call assertTrue(associated(" + args[0] + "," + args[1] + "), " + args[2] + ", &\n")
            #else:
                #p.outputFile.write("  call assertTrue(associated(" + args[0] + "," + args[1] + "), &\n")
        #else:
            #p.outputFile.write("  call assertTrue(associated(" + args[0] + "), &\n")
        #self.appendSourceLocation(p.outputFile, p.fileName, p.currentLineNumber)
        #p.outputFile.write(" )\n")
        #p.outputFile.write("  if (anyExceptions()) return\n")
        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber+1, p.fileName))


#class AtAssertNotAssociated(Action):
    #def __init__(self,parser):
        #self.parser = parser
        #self.name='@assertnotassociated'

    #def match(self, line):
        #m = re.match('\s*@assert(not|un)associated\s*\\((.*\w.*)\\)\s*$', line, re.IGNORECASE)
        #if m:
            #self.name='@assert'+m.groups()[0]+'associated'
        #else:
            #self.name='@assertnotassociated'

        #if not m:
            #m  = re.match( \
                #'\s*@assert(not|un)associated\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*),(.*\w*.*))\\)\s*$', \
                #line, re.IGNORECASE)

        ## How to get both (a,b) and (a,b,c) to match?
        #if not m:
            #m  = re.match( \
                #'\s*@assert(not|un)associated\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*))\\)\s*$', \
                #line, re.IGNORECASE)

        #if m:
            #self.name='@assert'+m.groups()[0]+'associated'
        #else:
            #self.name='@assertnotassociated'


        #return m

    #def appendSourceLocation(self, fileHandle, fileName, lineNumber):
        #fileHandle.write(" & location=SourceLocation( &\n")
        #fileHandle.write(" & '" + str(basename(fileName)) + "', &\n")
        #fileHandle.write(" & " + str(lineNumber) + ")")

    #def action(self, m, line):
        #p = self.parser

        ##-- args = parseArgsFirstRest('@assertassociated',line)
        ##ok args = parseArgsFirstSecondRest('@assertassociated',line)
        #args = parseArgsFirstSecondRest(self.name,line)

        ## print(9000,line)
        ## print(9001,args)

        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber, p.fileName))
        #if len(args) > 1:
            #if re.match('.*message=.*',args[1],re.IGNORECASE):
                #p.outputFile.write("  call assertFalse(associated(" + args[0] + "), " + args[1] + ", &\n")
            #elif len(args) > 2:
                #p.outputFile.write("  call assertFalse(associated(" + args[0] + "," + args[1] + "), " + args[2] + ", &\n")
            #else:
                #p.outputFile.write("  call assertFalse(associated(" + args[0] + "," + args[1] + "), &\n")
        #else:
            #p.outputFile.write("  call assertFalse(associated(" + args[0] + "), &\n")
        #self.appendSourceLocation(p.outputFile, p.fileName, p.currentLineNumber)
        #p.outputFile.write(" )\n")
        #p.outputFile.write("  if (anyExceptions()) return\n")
        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber+1, p.fileName))


#class AtAssertEqualUserDefined(Action):
    #"""Convenience directive replacing (a,b) with a call to assertTrue(a==b)
    #and an error message, if none is provided when invoked.
    #"""
    #def __init__(self,parser):
        #self.parser = parser

    #def match(self, line):
        #m  = re.match( \
            #'\s*@assertequaluserdefined\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*),(.*\w*.*))\\)\s*$', \
            #line, re.IGNORECASE)

        ## How to get both (a,b) and (a,b,c) to match?
        #if not m:
            #m  = re.match( \
                #'\s*@assertequaluserdefined\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*))\\)\s*$', \
                #line, re.IGNORECASE)

        #return m

    #def appendSourceLocation(self, fileHandle, fileName, lineNumber):
        #fileHandle.write(" & location=SourceLocation( &\n")
        #fileHandle.write(" & '" + str(basename(fileName)) + "', &\n")
        #fileHandle.write(" & " + str(lineNumber) + ")")

    #def action(self, m, line):
        #p = self.parser

        #args = parseArgsFirstSecondRest('@assertequaluserdefined',line)

        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber, p.fileName))
        #if len(args) > 2:
            #p.outputFile.write("  call assertTrue(" \
                               #+ args[0] + "==" + args[1] + ", " + args[2] + ", &\n")
        #else:
            #p.outputFile.write("  call assertTrue(" \
                               #+ args[0] + "==" + args[1] + ", &\n")
        #if not re.match('.*message=.*',line,re.IGNORECASE):
            #p.outputFile.write(" & message='<" + args[0] + "> not equal to <" + args[1] + ">', &\n")
        #self.appendSourceLocation(p.outputFile, p.fileName, p.currentLineNumber)
        #p.outputFile.write(" )\n")
        #p.outputFile.write("  if (anyExceptions()) return\n")
        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber+1, p.fileName))


#class AtAssertEquivalent(Action):
    #"""Convenience directive replacing (a,b) with a call to assertTrue(a.eqv.b)
    #and an error message, if none is provided when invoked.
    #"""
    #def __init__(self,parser):
        #self.parser = parser

    #def match(self, line):
        #m  = re.match( \
            #'\s*@assertequivalent\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*),(.*\w*.*))\\)\s*$', \
            #line, re.IGNORECASE)

        ## How to get both (a,b) and (a,b,c) to match?
        #if not m:
            #m  = re.match( \
                #'\s*@assertequivalent\s*\\((\s*([^,]*\w.*),\s*([^,]*\w.*))\\)\s*$', \
                #line, re.IGNORECASE)

        #return m

    #def appendSourceLocation(self, fileHandle, fileName, lineNumber):
        #fileHandle.write(" & location=SourceLocation( &\n")
        #fileHandle.write(" & '" + str(basename(fileName)) + "', &\n")
        #fileHandle.write(" & " + str(lineNumber) + ")")

    #def action(self, m, line):
        #p = self.parser

        #args = parseArgsFirstSecondRest('@assertequivalent',line)

        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber, p.fileName))
        #if len(args) > 2:
            #p.outputFile.write("  call assertTrue(" \
                               #+ args[0] + ".eqv." + args[1] + ", " + args[2] + ", &\n")
        #else:
            #p.outputFile.write("  call assertTrue(" \
                               #+ args[0] + ".eqv." + args[1] + ", &\n")
        #if not re.match('.*message=.*',line,re.IGNORECASE):
            #p.outputFile.write(" & message='<" + args[0] + "> not equal to <" + args[1] + ">', &\n")
        #self.appendSourceLocation(p.outputFile, p.fileName, p.currentLineNumber)
        #p.outputFile.write(" )\n")
        #p.outputFile.write("  if (anyExceptions()) return\n")
        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber+1, p.fileName))


#class AtMpiAssert(Action):
    #def __init__(self, parser):
        #self.parser = parser

    #def match(self, line):
        #m = re.match('\s*@mpiassert('+assertVariants+')\s*\\((.*\w.*)\\)\s*$', line, re.IGNORECASE)
        #return m

    #def appendSourceLocation(self, fileHandle, fileName, lineNumber):
        #fileHandle.write(" & location=SourceLocation( &\n")
        #fileHandle.write(" & '" + str(basename(fileName)) + "', &\n")
        #fileHandle.write(" & " + str(lineNumber) + ")")

    #def action(self, m, line):
        #p = self.parser

        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber, p.fileName))
        #p.outputFile.write("  call assert"+m.groups()[0]+"(" + m.groups()[1] + ", &\n")
        #self.appendSourceLocation(p.outputFile, p.fileName, p.currentLineNumber)
        #p.outputFile.write(" )\n")

        ## 'this' object may not exist if test is commented out.
        #if hasattr(p,'currentSelfObjectName'):
            #p.outputFile.write("  if (anyExceptions("+p.currentSelfObjectName+"%context)) return\n")
        #p.outputFile.write(cppSetLineAndFile(p.currentLineNumber+1, p.fileName))

#class AtBefore(Action):
    #def __init__(self, parser):
        #self.parser = parser

    #def match(self, line):
        #m = re.match('\s*@before\s*$', line, re.IGNORECASE)
        #return m

    #def action(self, m, line):
        #nextLine = self.parser.nextLine()
        #self.parser.userTestCase['setUp'] = getSubroutineName(nextLine)
        #self.parser.commentLine(line)
        #self.parser.outputFile.write(nextLine)

#class AtAfter(Action):
    #def __init__(self, parser):
        #self.parser = parser

    #def match(self, line):
        #m = re.match('\s*@after\s*$', line, re.IGNORECASE)
        #return m

    #def action(self, m, line):
        #nextLine = self.parser.nextLine()
        #self.parser.userTestCase['tearDown'] = getSubroutineName(nextLine)
        #self.parser.commentLine(line)
        #self.parser.outputFile.write(nextLine)

#class AtTestParameter(Action):
    #def __init__(self, parser):
        #self.parser = parser

    #def match(self, line):
        #m = re.match('\s*@testParameter\s*(|.*)$', line, re.IGNORECASE)
        #return m

    #def action(self, m, line):
        #options = re.match('\s*@testParameter\s*\\((.*)\\)\s*$', line, re.IGNORECASE)

        #self.parser.commentLine(line)
        #nextLine = self.parser.nextLine()
        #if not 'testParameterType' in self.parser.userTestCase:
            #self.parser.userTestCase['testParameterType'] = getTypeName(nextLine)
        #self.parser.outputFile.write(nextLine)

        #if options:
            #value = re.search('constructor\s*=\s*(\w*)', options.groups()[0], re.IGNORECASE)
            #if value:
                #self.parser.userTestCase['testParameterConstructor'] = value.groups()[0]
            #else:
                #self.parser.userTestCase['testParameterConstructor'] = self.parser.userTestCase['testParameterType']


#class AtIgnore(Action):
    #def __init__(self, parser):
        #self.parser = parser
        #self.keyword = '@ignore'

    #def match(self, line):
        #nameRe = "'\w+'|" + """\w+"""
        #m = re.match("\s*@ignore\s*$", line, re.IGNORECASE)
        #return m

    #def action(self, m, line):
        #print("Processing ignore:")
        #self.parser.current_method['ignore'] = True
        #self.parser.commentLine(line)
                
#class Parser():
    #def __init__(self, inputFileName, outputFileName):
        #def getBaseName(fileName):
            #from os.path import basename, splitext
            #base = basename(fileName)
            #return splitext(base)[0]

        #self.fileName = inputFileName
        #self.inputFile = open(inputFileName, 'r')
        #self.outputFile = open(outputFileName, 'w')
        #self.defaultSuiteName = getBaseName(inputFileName) + "_suite"
        #self.suiteName = ''

        #self.currentLineNumber = 0
        #self.userModuleName = '' # if any

        #self.userTestCase = {}
        #self.userTestCase['setUpMethod'] = ''
        #self.userTestCase['tearDownMethod'] = ''
        #self.userTestCase['defaultTestParameterNpes'] = [] # is MPI if not empty
        #self.userTestCase['defaultTestParametersExpr'] = ''
        #self.userTestCase['defaultTestParameterCases'] = []

        #self.userTestMethods = [] # each entry is a dictionary

        #self.wrapModuleName = "Wrap" + getBaseName(inputFileName)
        #self.currentLineNumber = 0

        #self.actions=[]
        #self.actions.append(AtTest(self))
        #self.actions.append(IsTestMethod(self))
        #self.actions.append(AtMpiTest(self))
        #self.actions.append(AtIgnore(self))
        #self.actions.append(AtTestCase(self))
        #self.actions.append(AtSuite(self))
        #self.actions.append(AtBegin(self))

        #self.actions.append(AtAssert(self))
        #self.actions.append(AtAssertAssociated(self))
##        self.actions.append(AtAssertAssociatedWith(self))
        #self.actions.append(AtAssertNotAssociated(self))
##        self.actions.append(AtAssertNotAssociatedWith(self))

        #self.actions.append(AtAssertEqualUserDefined(self))
        #self.actions.append(AtAssertEquivalent(self))

        #self.actions.append(AtMpiAssert(self))
        #self.actions.append(AtBefore(self))
        #self.actions.append(AtAfter(self))
        #self.actions.append(AtTestParameter(self))


    #def commentLine(self, line):
        #self.outputFile.write(re.sub('@','!@',line))

    #def run(self):
        #def parse(line):
            #for action in self.actions:
                #if (action.apply(line)): return
            #self.outputFile.write(line)

        #while True:
            #line = self.nextLine()
            #if  not line: break
            #parse(line)

        #if (not self.suiteName): self.suiteName = self.defaultSuiteName
        #if ('testParameterType' in self.userTestCase and (not 'constructor' in self.userTestCase)):
            #self.userTestCase['constructor'] = self.userTestCase['testParameterType']
        #self.makeWrapperModule()

    #def isComment(self, line):
        #return re.match('\s*(!.*|)$', line)

    #def nextLine(self):
        #while True:
            #self.currentLineNumber += 1
            #line = self.inputFile.readline()
            #if not line: break
            #if (self.isComment(line)):
                #self.outputFile.write(line)
                #pass
            #else:
                #break
        #return line


    #def printHeader(self):
        #self.outputFile.write('\n')
        #self.outputFile.write('module ' + self.wrapModuleName + '\n')
        #self.outputFile.write('   use FUnit\n')
        #if (self.userModuleName): self.outputFile.write('   use ' + self.userModuleName + '\n')
        #self.outputFile.write('   implicit none\n')
        #self.outputFile.write('   private\n\n')



    #def printTail(self):
        #self.outputFile.write('\n')
        #self.outputFile.write('end module ' + self.wrapModuleName + '\n\n')

    #def printWrapUserTestCase(self):
        #self.outputFile.write('   public :: WrapUserTestCase\n')
        #self.outputFile.write('   public :: makeCustomTest\n')
        #self.outputFile.write('   type, extends(' + self.userTestCase['type'] + ') :: WrapUserTestCase\n')
        #self.outputFile.write('      procedure(userTestMethod), nopass, pointer :: testMethodPtr\n')
        #self.outputFile.write('   contains\n')
        #self.outputFile.write('      procedure :: runMethod\n')
        #self.outputFile.write('   end type WrapUserTestCase\n\n')

        #self.outputFile.write('   abstract interface\n')
        #self.outputFile.write('     subroutine userTestMethod(this)\n')
        #if self.userModuleName:
            #self.outputFile.write('        use ' + self.userModuleName + '\n')
        #if 'type' in self.userTestCase:
            #self.outputFile.write('        class (' + self.userTestCase['type'] + '), intent(inout) :: this\n')
        #self.outputFile.write('     end subroutine userTestMethod\n')
        #self.outputFile.write('   end interface\n\n')

    #def printRunMethod(self):
        #self.outputFile.write('   subroutine runMethod(this)\n')
        #self.outputFile.write('      class (WrapUserTestCase), intent(inout) :: this\n\n')
        #self.outputFile.write('      call this%testMethodPtr(this)\n')
        #self.outputFile.write('   end subroutine runMethod\n\n')


    #def printParameterHeader(self, type):
        #self.outputFile.write('   type (' + type + '), allocatable :: testParameters(:)\n')
        #self.outputFile.write('   type (' + type + ') :: testParameter\n')
        #self.outputFile.write('   integer :: iParam \n')
        #self.outputFile.write('   integer, allocatable :: cases(:) \n')
        #self.outputFile.write(' \n')


    #def printMakeSuite(self):
        #self.outputFile.write('function ' + self.suiteName + '() result(suite)\n')
        #self.outputFile.write('   use FUnit\n')
        #if (self.userModuleName): self.outputFile.write('   use ' + self.userModuleName + '\n')
        #self.outputFile.write('   use '+ self.wrapModuleName + '\n')
        #self.outputFile.write('   type (TestSuite) :: suite\n\n')
        #self.outputFile.write('   class (Test), allocatable :: t\n\n')

        #if not self.userModuleName:
            #for testMethod in self.userTestMethods:
                #if ('ifdef' in testMethod):
                    #self.outputFile.write('#ifdef ' + testMethod['ifdef'] + '\n')
                #elif ('ifndef' in testMethod):
                    #self.outputFile.write('#ifndef ' + testMethod['ifndef'] + '\n')
                #self.outputFile.write('   external ' + testMethod['name'] + '\n')
                #if ('ifdef' in testMethod or 'ifndef' in testMethod):
                    #self.outputFile.write('#endif\n')
            #self.outputFile.write('\n')
            #if 'setUp' in self.userTestCase:
                #self.outputFile.write('   external ' + self.userTestCase['setUp'] + '\n')
            #if 'tearDown' in self.userTestCase:
                #self.outputFile.write('   external ' + self.userTestCase['tearDown'] + '\n')
            #self.outputFile.write('\n')

        #if 'testParameterType' in self.userTestCase:
            #type = self.userTestCase['testParameterType']
            #self.printParameterHeader(type)

        #self.outputFile.write("   suite = TestSuite('" + self.suiteName + "')\n\n")

        #for testMethod in self.userTestMethods:
            #if ('ifdef' in testMethod):
                #self.outputFile.write('#ifdef ' + testMethod['ifdef'] + '\n')
            #elif ('ifndef' in testMethod):
                #self.outputFile.write('#ifndef ' + testMethod['ifndef'] + '\n')
            #if 'type' in self.userTestCase:
                #self.addUserTestMethod(testMethod)
            #else:
                #if 'npRequests' in testMethod:
                    #self.addMpiTestMethod(testMethod)
                #else: # vanilla
                    #self.addSimpleTestMethod(testMethod)
            #self.outputFile.write('\n')
            #if ('ifdef' in testMethod or 'ifndef' in testMethod):
                #self.outputFile.write('#endif\n')

        #self.outputFile.write('\nend function ' + self.suiteName + '\n\n')

    #def addSimpleTestMethod(self, testMethod):
        #args = "'" + testMethod['name'] + "', " + testMethod['name']
        #if 'setUp' in testMethod:
            #args += ', ' + testMethod['setUp']
        #elif 'setUp' in self.userTestCase:
            #args += ', ' + self.userTestCase['setUp']

        #if 'tearDown' in testMethod:
            #args += ', ' + testMethod['tearDown']
        #elif 'tearDown' in self.userTestCase:
            #args += ', ' + self.userTestCase['tearDown']

        #if 'type' in testMethod:
            #type =  testMethod['type']
        #else:
            #type = 'TestMethod'

        #self.outputFile.write('   t = ' + type + '(' + args + ')\n')
        #if ('ignore' in testMethod):
            #self.outputFile.write('   call t%insert(Ignore%type_name(),Ignore)\n')
        #self.outputFile.write('   call suite%addTest(t)\n')

    #def addMpiTestMethod(self, testMethod):
        #for npes in testMethod['npRequests']:
            #args = "'" + testMethod['name'] + "', " + testMethod['name'] + ", " + str(npes)
            #if 'setUp' in testMethod:
                #args += ', ' + testMethod['setUp']
            #elif 'setUp' in self.userTestCase:
                #args += ', ' + self.userTestCase['setUp']

            #if 'tearDown' in testMethod:
                #args += ', ' + testMethod['tearDown']
            #elif 'tearDown' in self.userTestCase:
                #args += ', ' + self.userTestCase['tearDown']

            #if 'type' in testMethod:
                #type =  testMethod['type']
            #else:
                #type = 'newMpiTestMethod'
#<<<<<<< HEAD:bin/pFUnitParser.py
                    
            #self.outputFile.write('   t = ' + type + '(' + args + ')\n')
            #if ('ignore' in testMethod):
                #self.outputFile.write('   call t%insert(Ignore%type_name(),Ignore)\n')
            #self.outputFile.write('   call suite%addTest(t)\n')

#=======

            #self.outputFile.write('   call suite%addTest(' + type + '(' + args + '))\n')
#>>>>>>> Renamed the processing tool and split the backend into a module. Updated the testing to work again.:bin/funit/parser.py


    #def addUserTestMethod(self, testMethod):

        #args = "'" + testMethod['name'] + "', " + testMethod['name']
        #if 'npRequests' in testMethod:
            #npRequests = testMethod['npRequests']
        #else:
            #if 'npRequests' in self.userTestCase:
                #npRequests = self.userTestCase['npRequests']
            #else:
                #npRequests = [1]

        #if 'cases' in testMethod:
            #cases = testMethod['cases']
        #elif 'cases' in self.userTestCase:
            #cases = self.userTestCase['cases']

        #testParameterArg = '' # unless

        #if 'cases' in locals():
            #testParameterArg = ', testParameter'
            #self.outputFile.write('   cases = ' + testMethod['cases'] + '\n')
            #self.outputFile.write('   testParameters = [(' +
                                  #self.userTestCase['testParameterConstructor'] +
                                  #'(cases(iCase)), iCase = 1, size(cases))]\n\n')

        #if 'testParameterType' in self.userTestCase:
            #if 'testParameters' in testMethod:
                #testParameters = testMethod['testParameters']
            #elif 'testParameters' in self.userTestCase:
                #testParameters = self.userTestCase['testParameters']

        #isMpiTestCase = 'npRequests' in self.userTestCase
        #isMpiTestCase = isMpiTestCase or any('npRequests' in testMethod for testMethod in self.userTestMethods)

        #if 'testParameters' in locals():
            #testParameterArg = ', testParameter'
            #self.outputFile.write('   testParameters = ' + testParameters + '\n\n')
        #elif isMpiTestCase:
            #testParameterArg = ', testParameter'


        #for npes in npRequests:

            #if 'testParameters' in locals() or 'cases' in locals():
                #self.outputFile.write('   do iParam = 1, size(testParameters)\n')
                #self.outputFile.write('      testParameter = testParameters(iParam)\n')

            #if isMpiTestCase:
                #self.outputFile.write('   call testParameter%setNumProcessesRequested(' + str(npes) + ')\n')

            #self.outputFile.write('   call suite%addTest(makeCustomTest(' +
                                  #args + testParameterArg + '))\n')
            #if 'cases' in locals() or 'testParameters' in locals():
                #self.outputFile.write('   end do\n')



    #def printMakeCustomTest(self, isMpiTestCase):
        #args = 'methodName, testMethod'
        #declareArgs =  '#ifdef INTEL_13\n'
        #declareArgs +=  '      use FUnit, only: testCase\n'
        #declareArgs +=  '#endif\n'
        #declareArgs +=  '      type (WrapUserTestCase) :: aTest\n'
        #declareArgs +=  '#ifdef INTEL_13\n'
        #declareArgs +=  '      target :: aTest\n'
        #declareArgs +=  '      class (WrapUserTestCase), pointer :: p\n'
        #declareArgs +=  '#endif\n'
        #declareArgs += '      character(len=*), intent(in) :: methodName\n'
        #declareArgs += '      procedure(userTestMethod) :: testMethod\n'

        #if 'testParameterType' in self.userTestCase:
            #args += ', testParameter'
            #declareArgs += '      type (' + self.userTestCase['testParameterType'] + '), intent(in) :: testParameter\n'

        #self.outputFile.write('   function makeCustomTest(' + args + ') result(aTest)\n')
        #self.outputFile.write(declareArgs)

        #if 'constructor' in self.userTestCase:
            #if 'testParameterType' in self.userTestCase:
                #constructor = self.userTestCase['constructor'] + '(testParameter)'
            #else:
                #constructor = self.userTestCase['constructor'] + '()'
            #self.outputFile.write('      aTest%' + self.userTestCase['type'] + ' = ' + constructor + '\n\n')

        #self.outputFile.write('      aTest%testMethodPtr => testMethod\n')

        #self.outputFile.write('#ifdef INTEL_13\n')
        #self.outputFile.write('      p => aTest\n')
        #self.outputFile.write('      call p%setName(methodName)\n')
        #self.outputFile.write('#else\n')
        #self.outputFile.write('      call aTest%setName(methodName)\n')
        #self.outputFile.write('#endif\n')

        #if 'testParameterType' in self.userTestCase:
            #self.outputFile.write('      call aTest%setTestParameter(testParameter)\n')


        #self.outputFile.write('   end function makeCustomTest\n')

    #def makeWrapperModule(self):
        ##-----------------------------------------------------------
        ## ! Start here
        #self.printHeader()

        #if 'type' in self.userTestCase:
            #self.printWrapUserTestCase()

        #self.outputFile.write('contains\n\n')

        #if 'type' in self.userTestCase:
            #self.printRunMethod()

        #if 'type' in self.userTestCase:
            #isMpiTestCase = 'npRequests' in self.userTestCase
            #isMpiTestCase = isMpiTestCase or any('npRequests' in testMethod for testMethod in self.userTestMethods)
            #if isMpiTestCase and not 'testParameterType' in self.userTestCase:
                #self.userTestCase['testParameterType'] = 'MpiTestParameter'

            #self.printMakeCustomTest(isMpiTestCase)

        #self.printTail()
        #self.printMakeSuite()

    #def final(self):
        #self.inputFile.close()
        #self.outputFile.close()
