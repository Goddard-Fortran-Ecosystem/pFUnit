"""Tests for parser utility functions."""

import unittest
import sys

sys.path.append("..")
from funit.pFUnitParser import (
    cppSetLineAndFile,
    getSubroutineName,
    getSelfObjectName,
    getTypeName,
    parseArgsFirstRest,
    parseArgsFirstSecondRest,
)


class TestParserUtils(unittest.TestCase):
    def testCppSetLineAndFile(self):
        self.assertEqual('#line 7 "foo"\n', cppSetLineAndFile(7, "foo"))
        self.assertEqual('#line 3 "bar"\n', cppSetLineAndFile(3, "bar"))

    def testGetSubroutineName(self):
        self.assertEqual("a", getSubroutineName("subroutine a()"))
        self.assertEqual("abcd", getSubroutineName("subroutine   abcd ()"))

    def testGetSelfObjectName(self):
        self.assertEqual("b", getSelfObjectName("subroutine a(b)"))
        self.assertEqual("bc", getSelfObjectName("subroutine a(bc)"))
        self.assertEqual("bc", getSelfObjectName("subroutine a(bc,d)"))
        self.assertEqual("bc", getSelfObjectName("subroutine a(bc, d)"))
        self.assertEqual("bc", getSelfObjectName("subroutine a(bc ,d)"))
        self.assertEqual("bc", getSelfObjectName("subroutine a(bc , d)"))

    def testGetTypeName(self):
        self.assertEqual("foo", getTypeName(" type :: foo"))
        self.assertEqual("foo", getTypeName(" type, extends(something) :: foo"))
        self.assertEqual("foo", getTypeName(" type, abstract :: foo"))
        self.assertEqual(
            "foo", getTypeName(" type, extends(something), abstract :: foo")
        )

    def testParseArgsFirstRest(self):
        """Test that the first-rest argument parsing is adequate."""
        self.assertEqual(["a1", "b1"], parseArgsFirstRest("", "a1,b1"))
        self.assertEqual(["a4()", "b4"], parseArgsFirstRest("", "a4(),b4"))
        self.assertEqual(["a4%z()", "b4"], parseArgsFirstRest("", "a4%z(),b4"))
        self.assertEqual(["a4", "b4%z()"], parseArgsFirstRest("", "a4,b4%z()"))
        self.assertEqual(["a10", "b10,c10"], parseArgsFirstRest("", "a10,b10,c10"))
        self.assertEqual(
            ["a2"], parseArgsFirstRest("@assertassociated", "@assertassociated(a2)")
        )
        self.assertEqual(
            ["a3", "b3,message='This is the message.'"],
            parseArgsFirstRest(
                "@assertassociated",
                "@assertassociated(a3,b3,message='This is the message.')",
            ),
        )
        self.assertEqual(
            ["a", "b,c,d"],
            parseArgsFirstRest("@assertassociated", "@assertassociated(a,b,c,d)"),
        )

    def testParseArgsFirstSecondRest(self):
        """Test that the first-second-rest argument parsing is adequate."""
        self.assertEqual(
            None, parseArgsFirstSecondRest("@assertassociated", "@assertassociated")
        )
        self.assertEqual(
            None, parseArgsFirstSecondRest("@assertassociated", "@assertassociated()")
        )
        self.assertEqual(
            ["a"], parseArgsFirstSecondRest("@assertassociated", "@assertassociated(a)")
        )
        self.assertEqual(
            ["a", "b"],
            parseArgsFirstSecondRest("@assertassociated", "@assertassociated(a,b)"),
        )
        self.assertEqual(
            ["a", "b", "message='This is the message.'"],
            parseArgsFirstSecondRest(
                "@assertassociated",
                "@assertassociated(a,b,message='This is the message.')",
            ),
        )
        self.assertEqual(
            ["a", "b%z()", "c,d"],
            parseArgsFirstSecondRest(
                "@assertassociated", "@assertassociated(a,b%z(),c,d)"
            ),
        )
        self.assertEqual(["a4", "b4", "c4"], parseArgsFirstSecondRest("", "a4,b4,c4"))


if __name__ == "__main__":
    unittest.main()
