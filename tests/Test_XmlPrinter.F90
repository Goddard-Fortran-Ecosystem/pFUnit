#include "reflection.h"
module Test_XmlPrinter_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
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
      use Utilities_mod, only: newUnit
      use TestResult_mod, only: TestResult, newTestResult

      type (TestResult) :: aResult
      type (SimpleTestCase) :: aTest, aTest2
      type (XmlPrinter) :: printer
      integer :: iostat, stat, cstat, xmlUnit, outUnit
      character(len=200) :: fileName, suiteName, command, &
           xsdPath, outFile, errMsg

      xmlUnit = newUnit()
      outUnit = newUnit()
      fileName = 'test.xml'
      suiteName = 'suitename<<>>""'
      xsdPath = 'tests/junit-4.xsd'
      outFile = 'tests/test_xmlprinter_output.tmp'

      open(unit=xmlUnit, file=fileName, iostat=iostat)
      call assertEqual(iostat, 0, 'Could not open XML file')

      printer = newXmlPrinter(xmlUnit)

      call aTest%setSurrogate()
      call aTest%setName('failtest<>"')
      call aTest2%setSurrogate()
      call aTest2%setName('successtest<>"')

      aResult = newTestResult()
      call aResult%addFailure(aTest%getSurrogate(), [newException('<invalid>')])
      call aResult%addFailure(aTest%getSurrogate(), [newException('"test"')])
      call aResult%addSuccess(aTest2%getSurrogate())

      call aResult%setName(suiteName)
      call printer%print(aResult)
      close(xmlUnit)

      command = 'xmllint --noout --nowarning --schema ' // trim(xsdPath) &
           // ' ' // trim(fileName) // ' 2> ' // outFile
      stat = system(command)
      if(stat /= 0) then
         open(unit=outUnit, file=outFile, iostat=iostat, &
              status='old', action='read')
         call assertEqual(iostat, 0, 'XML validation failed, unknown cause')
         read(outUnit, '(a)') errMsg
         close(outUnit)
         call assertEqual(stat, 0, 'XML validation failed: ' // errMsg)
      end if

   end subroutine testValidXml

end module Test_XmlPrinter_mod
