#!/usr/bin/python
# For python 2.6-2.7
from __future__ import print_function

from os.path import *
import re

class MyError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

assertVariants = 'Equal|True|False|LessThan|LessThanOrEqual|GreaterThan|GreaterThanOrEqual'
assertVariants += '|IsMemberOf|Contains|Any|All|NotAll|None|IsPermutationOf'
assertVariants += '|ExceptionRaised|SameShape|IsNaN|IsFinite'

def cppSetLineAndFile(line, file):
    return "#line " + str(line) + ' "' + file + '"\n'

def getSubroutineName(line):
    try:
        m = re.match('\s*subroutine\s+(\w*)\s*(\\([\w\s,]*\\))?\s*(!.*)*$', line, re.IGNORECASE)
        return m.groups()[0]
    except:
        raise MyError('Improper format in declaration of test procedure.')


def getSelfObjectName(line):
    m = re.match('\s*subroutine\s+\w*\s*\\(\s*(\w+)\s*(,\s*\w+\s*)*\\)\s*$', line, re.IGNORECASE)
    return m.groups()[0]

def getTypeName(line):
    m = re.match('\s*type(.*::\s*|\s+)(\w*)\s*$', line, re.IGNORECASE)
    return m.groups()[1]
 
class Action():
    def apply(self, line):
        m = self.match(line)
        if m: self.action(m, line)
        return m

#------------------
class AtTest(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@test(\s*(\\(.*\\))?\s*$)', line, re.IGNORECASE)
        return m

    def action(self, m, line):
        nextLine = self.parser.nextLine()
        self.parser.tests.append({'name':getSubroutineName(nextLine)})
        self.parser.outputFile.write("!"+line)
        self.parser.outputFile.write(nextLine)

#------------------
class AtMpiTest(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@mpitest\s*\\(.*npes\s*=\s*\\[([0-9,\s]+)\\].*\\)\s*$', line, re.IGNORECASE)
        return m

    def action(self, m, line):
        args = re.match('\s*@mpitest\s*\\((.*)\\)\s*$', line, re.IGNORECASE).groups()[0]
        dictionary = {}

        # npes is mandatory
        npesString = re.search('npes=\\[([0-9,\s]+)\\]', args, re.IGNORECASE).groups()[0]
        npes = map(int, npesString.split(','))
        dictionary['npes'] = npes

        #ifdef is optional
        matchIfdef = re.match('.*ifdef\s*=\s*(\w+)', args, re.IGNORECASE)

        if (matchIfdef): 
            ifdef = matchIfdef.groups()[0]
            dictionary['ifdef'] = ifdef


        nextLine = self.parser.nextLine()
        dictionary['name'] = getSubroutineName(nextLine)
        # save "self" name for use with @mpiAssert
        self.parser.currentSelfObjectName = getSelfObjectName(nextLine)

        self.parser.mpitests.append(dictionary)
        self.parser.outputFile.write("!"+line)
        self.parser.outputFile.write(nextLine)

class AtTestCase(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@testcase\s*$', line, re.IGNORECASE)
        return m
    
    def action(self, m, line):
        nextLine = self.parser.nextLine()
        self.parser.testCase=getTypeName(nextLine)
        self.parser.outputFile.write("!"+line)
        self.parser.outputFile.write(nextLine)


class AtSuite(Action):
    def __init__(self, parser):
        self.parser = parser
    def match(self, line):
        nameRe = "'\w+'|" + """\w+"""
        m = re.match("\s*@suite\s*\\(\s*name\s*=\s*("+nameRe+")\s*\\)\s*$", line, re.IGNORECASE)
        return m

    def action(self, m, line):
        self.parser.suiteName=m.groups()[0][1:-1]


class AtBegin(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*module\s+(\w*)\s*$', line, re.IGNORECASE)
        return m

    def action(self, m, line):
        self.parser.moduleName = m.groups()[0]
        if (not self.parser.suiteName): self.parser.suiteName = self.parser.moduleName+"_suite"
        self.parser.outputFile.write(line)


class AtAssert(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@assert('+assertVariants+')\s*\\((.*\w.*)\\)\s*$', line, re.IGNORECASE)
        return m

    def appendSourceLocation(self, fileHandle, fileName, lineNumber):
        fileHandle.write(" & location=SourceLocation( &\n")
        fileHandle.write(" & '" + str(basename(fileName)) + "', &\n")
        fileHandle.write(" & " + str(lineNumber) + ")")

    def action(self, m, line):
        p = self.parser
        
        p.outputFile.write(cppSetLineAndFile(p.lineNumber, p.fileName))
        p.outputFile.write("  call assert"+m.groups()[0]+"(" + m.groups()[1] + ", &\n")
        self.appendSourceLocation(p.outputFile, p.fileName, p.lineNumber)
        p.outputFile.write(" )\n")
        p.outputFile.write("  if (anyExceptions()) return\n")
        p.outputFile.write(cppSetLineAndFile(p.lineNumber+1, p.fileName))

class AtMpiAssert(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@mpiassert('+assertVariants+')\s*\\((.*\w.*)\\)\s*$', line, re.IGNORECASE)
        return m

    def appendSourceLocation(self, fileHandle, fileName, lineNumber):
        fileHandle.write(" & location=SourceLocation( &\n")
        fileHandle.write(" & '" + str(basename(fileName)) + "', &\n")
        fileHandle.write(" & " + str(lineNumber) + ")")

    def action(self, m, line):
        p = self.parser
        
        p.outputFile.write(cppSetLineAndFile(p.lineNumber, p.fileName))
        p.outputFile.write("  call assert"+m.groups()[0]+"(" + m.groups()[1] + ", &\n")
        self.appendSourceLocation(p.outputFile, p.fileName, p.lineNumber)
        p.outputFile.write(" )\n")
        
        p.outputFile.write("  if (anyExceptions("+p.currentSelfObjectName+"%context)) return\n")
        p.outputFile.write(cppSetLineAndFile(p.lineNumber+1, p.fileName))

class AtBefore(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@before\s*$', line, re.IGNORECASE)
        return m 

    def action(self, m, line):
        nextLine = self.parser.nextLine()
        self.parser.setUp = getSubroutineName(nextLine)
        self.parser.outputFile.write("!"+line)
        self.parser.outputFile.write(nextLine)

class AtAfter(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@after\s*$', line, re.IGNORECASE)
        return m 

    def action(self, m, line):
        nextLine = self.parser.nextLine()
        self.parser.tearDown = getSubroutineName(nextLine)
        self.parser.outputFile.write("!"+line)
        self.parser.outputFile.write(nextLine)

class AtParameters(Action):
    def __init__(self, parser):
        self.parser = parser

    def match(self, line):
        m = re.match('\s*@parameters\s*=\s*\\[(.*)\\]\s*$', line, re.IGNORECASE)
        return m

    def action(self, m, line):
        self.parser.parameters=m.groups()[0].split(',')
        self.parser.outputFile.write("!"+line)
        nextLine = self.parser.nextLine()
        self.parser.parameterType = getTypeName(nextLine)
        self.parser.outputFile.write(nextLine)



class Parser():
    def __init__(self, inputFileName, outputFileName):
        def getBaseName(fileName):
            from os.path import basename, splitext
            base = basename(fileName)
            return splitext(base)[0]

        self.inputFile = open(inputFileName, 'r')
        self.outputFile = open(outputFileName, 'w')
        self.moduleName = ''
        self.suiteName = ''
        self.testCase = ''
        self.setUp = ''
        self.tearDown = ''
        self.defaultName = getBaseName(inputFileName) + "_suite"
        self.fileName = inputFileName
        self.lineNumber = 0
        self.parameters=[]
        self.parameterType = ''
        self.tests=[]
        self.mpitests=[]
        self.actions=[]

        self.actions.append(AtTest(self))
        self.actions.append(AtMpiTest(self))
        self.actions.append(AtTestCase(self))
        self.actions.append(AtSuite(self))
        self.actions.append(AtBegin(self))
        self.actions.append(AtAssert(self))
        self.actions.append(AtMpiAssert(self))
        self.actions.append(AtBefore(self))
        self.actions.append(AtAfter(self))
        self.actions.append(AtParameters(self))

    def run(self):
        def parse(line):
            for action in self.actions:
                if (action.apply(line)): return
            self.outputFile.write(line)

        while True:
            line = self.nextLine()
            if  not line: break
            parse(line)
        self.makeSuite()

    def isComment(self, line):
        return re.match('\s*(!.*|)$', line)

    def nextLine(self):
        while True:
            self.lineNumber += 1
            line = self.inputFile.readline()
            if not line: break
            if (self.isComment(line)):
                self.outputFile.write(line)
                pass
            else:
                break
        return line


    def makeSuite(self):
        def printHeader(file, suiteName, moduleName):
            file.write('\n')
            file.write('\n')
            file.write('\n')
            file.write('function ' + suiteName +'() result(suite)\n')
            file.write('   use pFUnit_mod\n')
            if (moduleName): file.write('   use ' + moduleName + '\n')
            file.write('   implicit none\n\n')
            file.write('   type (TestSuite) :: suite\n')

        def printTail(file, suiteName):
            file.write('\n')
            file.write('end function ' + suiteName + '\n\n')
            
        def printParameterHeader(file, type, constructor):
            file.write('   type (' + type + '), allocatable :: parameters(:) \n')
            file.write('   type (' + constructor + ') :: dummy \n')
            file.write('   integer :: iParam \n')
            file.write(' \n')

        def startParamLoop(file, parameters):
            file.write('   parameters = dummy%getParameters()\n')
            file.write(' \n')
            file.write('   do iParam = 1, size(parameters) \n')
            file.write('      associate( & \n')
            parameter = parameters[0]
            file.write('         & '+parameter+' => parameters(iParam)%'+parameter)
            for parameter in parameters[1:]:
                file.write(' ,&\n         & '+parameter+' => parameters(iParam)%'+parameter)
            file.write('  &\n         & )\n')

        def endParamLoop(file):
            file.write('      end associate \n')
            file.write('   end do \n')
            file.write(' \n')

        # Create string that contains additional arguments needed by
        # constructor for nontrivial cases
        def makeExtraArguments(setUp, tearDown, parameters):
            fixtureArguments = ''
            if (setUp):
                fixtureArguments += ',' + setUp
            if tearDown:
                fixtureArguments += ',' + tearDown
            extraArguments = fixtureArguments
                    
            if parameters:
                paramArguments = ''
                for parameter in parameters:
                    paramArguments += ',' + parameter
                extraArguments += paramArguments

            return extraArguments

        #-----------------------------------------------------------
        # ! Start here
        if (self.testCase):
            constructor=self.testCase
        else:
            constructor = 'newTestMethod'
        if (not self.suiteName): self.suiteName = self.defaultName

        printHeader(self.outputFile, self.suiteName, self.moduleName)
        if not self.moduleName:
            for test in self.tests:
                self.outputFile.write("   external " + test['name'] + "\n")
            for test in self.mpitests:
                self.outputFile.write("   external " + test['name'] + "\n")

        if self.parameters:
            printParameterHeader(self.outputFile, self.parameterType, constructor)

        self.outputFile.write("   suite = newTestSuite('" + self.suiteName + "')\n\n")

        extraArguments = makeExtraArguments(self.setUp, self.tearDown, self.parameters)

        for test in self.tests:
            name = test['name']

            if self.parameters:
                startParamLoop(self.outputFile, self.parameters)
                indent = '      '
            else:
                indent = ''

            self.outputFile.write(indent + 
                                  '   call suite%addTest(' + constructor + 
                                  '(' + "'" + name + "'" + ',' + name + extraArguments + '))\n')

            if self.parameters:
                endParamLoop(self.outputFile)

        if (self.testCase):
            constructor=self.testCase
        else:
            constructor = 'newMpiTestMethod'

        for test in self.mpitests:
            if ('ifdef' in test):
                self.outputFile.write('#ifdef ' + test['ifdef'] + '\n')
            name = test['name']
            if self.parameters:
                startParamLoop(self.outputFile, self.parameters)
                indent = '      '
            else:
                indent = ''

            for np in test['npes']:
                extraArgs = ',' + str(np) + extraArguments
                self.outputFile.write(indent + 
                                      '   call suite%addTest(' + constructor + 
                                      '('+"'" + name + "'" + ',' + name + extraArgs + '))\n')
            if self.parameters:
                endParamLoop(self.outputFile)
            if ('ifdef' in test):
                self.outputFile.write('#endif ' + test['ifdef'] + '\n')

        printTail(self.outputFile, self.suiteName)

    def final(self):
        self.inputFile.close()
        self.outputFile.close()

if __name__ == "__main__":
    import sys
    print("Processing file", sys.argv[1])
    p = Parser(sys.argv[1], sys.argv[2])
    p.run()
    p.final()
    print(" ... Done.  Results in", sys.argv[2])


