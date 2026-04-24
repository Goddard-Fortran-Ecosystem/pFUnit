"""Shared test fixtures and mocks for parser tests."""

import sys

sys.path.append("..")

from funit.pFUnitParser import Parser


class MockWriter:
    """Mock file writer that captures output lines."""

    def __init__(self, parser):
        self.parser = parser

    def write(self, line):
        self.parser.outLines.append(line)


class MockParser(Parser):
    """Mock parser for testing that doesn't require actual files."""

    def __init__(self, lines):
        self.saveLines = lines
        self.lines = self.saveLines[:]
        self.outputFile = MockWriter(self)
        self.outLines = []
        self.userTestCase = {}
        self.userTestMethods = []
        self.currentSelfObjectName = ""

    def nextLine(self):
        while True:
            line = self.lines.pop(0)
            if self.isComment(line):
                pass
            else:
                break
        return line

    def reset(self):
        self.lines = self.saveLines[:]
