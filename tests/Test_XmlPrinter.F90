#include "reflection.h"
module Test_XmlPrinter_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   use TestResult_mod, only: TestResult, newTestResult
   use TestCase_mod
   use SimpleTestCase_mod, only: newSimpleTestCase, SimpleTestCase
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use TestResult_mod, only: TestResult, newTestResult
      use TestCase_mod
      use TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('TestXmlPrinterSuite')

#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

      ADD(testValidXml)

   end function suite

   subroutine testValidXml()
      use Assert_mod, only: assertEqual
      use Exception_mod, only: newException
      use SimpleTestCase_mod, only: SimpleTestCase
      use SurrogateTestCase_mod
      use TestCase_mod
      use XmlPrinter_mod, only: XmlPrinter, newXmlPrinter

      type (TestResult) :: aResult
      type (SimpleTestCase) :: aTest, aTest2
      type (XmlPrinter) :: printer
      integer, parameter :: unit = 22
      integer :: iostat, stat, cstat
      character(len=200) :: fileName, suiteName, command, xsdPath

      fileName = 'test.xml'
      suiteName = 'suitename'
      xsdPath = 'tests/junit-4.xsd'

      open(unit=unit, file=fileName, iostat=iostat)
      call assertEqual(iostat, 0, 'Could not open XML file')

      printer = newXmlPrinter(unit)

      call aTest%setSurrogate()
      call aTest%setName('failtest<>"')
      call aTest2%setSurrogate()
      call aTest2%setName('successtest<>"')

      aResult = newTestResult()
      call aResult%addFailure(aTest%getSurrogate(), [newException('<invalid>')])
      call aResult%addFailure(aTest%getSurrogate(), [newException('"test"')])
      call aResult%addSuccess(aTest2%getSurrogate())

      call printer%print(suiteName, aResult)
      close(unit)

      command = 'xmllint --noout --nowarning --schema ' // trim(xsdPath) &
           // ' ' // trim(fileName) // ' 2>/dev/null'
      stat = system(command)
      call assertEqual(stat, 0, 'XML invalid')

   end subroutine testValidXml

end module Test_XmlPrinter_mod
