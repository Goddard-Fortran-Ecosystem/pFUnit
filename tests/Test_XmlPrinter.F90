!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group.
!-------------------------------------------------------------------------------
!  MODULE: Test_XmlPrinter
!
!> @brief
!! Output test messages in junit.xsd-compatible XML.
!!
!! @author
!! Halvor Lund
!!
!! @date
!! 2014 July
!! 
!! @note Set up a test failure and feed it to an XML-based printer so
!! that we can test its output. Use command line call 
!! to try to find "xmllint," and if available, use it to validate the
!! output against junit.xsd.  Either way, check the output against a
!! hard-coded expected result (a regression test).
!
! REVISION HISTORY:
!
! 2014 August 7. Added regression test and Intel support. MLR.
! 2014 July. Initial commit.
!
!-------------------------------------------------------------------------------
!#include "reflection.h"
module Test_XmlPrinter_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestCase_mod
      use PF_TestMethod_mod, only: newTestMethod
      use PF_Test_mod
      use PF_TestResult_mod
      use PF_TestSuite_mod, only: TestSuite, newTestSuite
      type (TestSuite) :: suite

      suite = newTestSuite('TestXmlPrinterSuite')

!#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

      call suite%addTest( &
           &   newTestMethod('testValidXml', &
           &                  testValidXml))

   end function suite

   subroutine testValidXml()
      use PF_Assert_mod, only: assertEqual
      use PF_Exception_mod, only: Exception
      use PF_ExceptionList_mod
      use PF_TestCase_mod
      use SimpleTestCase_mod, only: SimpleTestCase
      use PF_SurrogateTestCase_mod
      use PF_TestResult_mod, only: TestResult, newTestResult
      use PF_XmlPrinter_mod, only: XmlPrinter, newXmlPrinter

      type (TestResult) :: aResult
      type (SimpleTestCase), target :: aTest, aTest2
      type (XmlPrinter) :: printer
      integer :: iostat, stat, cstat, xmlUnit, outUnit
      character(len=200) :: fileName, suiteName, command, &
           xsdPath, outFile, errMsg
      type (ExceptionList) :: list

      fileName = 'test.xml'
      suiteName = 'suitename<<>>""'
      xsdPath = 'tests/junit-4.xsd'
      outFile = 'tests/test_xmlprinter_output.tmp'

      open(newunit=xmlUnit, file=fileName, iostat=iostat)
      call assertEqual(iostat, 0, 'Could not open XML file')

      printer = newXmlPrinter(xmlUnit)

      call aTest%setSurrogate()
      call aTest%setName('failtest<>"')
      call aTest2%setSurrogate()
      call aTest2%setName('successtest<>"')

      aResult = newTestResult()
      call list%throw(Exception('<invalid>'))
      call aResult%addFailure(aTest%getSurrogate(), list)
      call list%clear()
      call list%throw(Exception('"test"'))
      call aResult%addFailure(aTest%getSurrogate(), list)
      call aResult%addSuccess(aTest2%getSurrogate())

      call aResult%setName(suiteName)
      call printer%print(aResult)
      close(xmlUnit)

      ! Validate the file against the de facto JUnit xsd.
      ! If xmlint not found, just move on quietly.
      command = 'xmllint --version > /dev/null 2>&1'
      call execute_command_line(command,exitstat=stat)

      if (stat == 0) then
         command = 'xmllint --noout --nowarning --schema ' // trim(xsdPath) &
              // ' ' // trim(fileName) // ' 2> ' // trim(outFile)
         call execute_command_line(command,exitstat=stat)
         if(stat /= 0) then
            open(newunit=outUnit, file=outFile, iostat=iostat, &
                 status='old', action='read')
            call assertEqual(iostat, 0, 'XML validation failed, unknown cause')
            read(outUnit, '(a)') errMsg
            close(outUnit)
            call assertEqual(stat, 0, 'XML validation failed: ' // errMsg)
         end if
      end if

      ! Compare the output to our "gold standard" regression test.
      !
      call compareXMLFileToExpectation(fileName)

   end subroutine testValidXml

   subroutine compareXMLFileToExpectation(xmlFile)
     use PF_Assert_mod, only: assertEqual
     use PF_Assert_mod, only: assertTrue
     use iso_fortran_env, only: iostat_end

     character(len=200), intent(in) :: xmlFile
     integer :: iostat, xmlUnit, iExpectedLine

     character(len=100) :: xmlFileLine
     character(len=100), dimension(9) :: expected 

     expected=(/ character(len=100) :: &
#ifndef PGI
'<testsuite name="suitename[[]]''''" errors="0" failures="2" tests="0" time=".0000">', &
#else
'<testsuite name="suitename[[]]''''" errors="0" failures="2" tests="0"&
& time="0.0000">', &
#endif
'<testcase name="successtest[]''"/>', &
'<testcase name="failtest[]''">', &
'<failure message="Location: [[unknown location]], [invalid] "/>', &
'</testcase>', &
'<testcase name="failtest[]''">', &
'<failure message="Location: [[unknown location]], ''test'' "/>', &
'</testcase>', &
'</testsuite>' /)

     open(newunit=xmlUnit, file=xmlFile, iostat=iostat, &
          & status='old', action='read')
     call assertEqual(iostat, 0, 'Could not open XML file for testing.')

     iExpectedLine = 0
     do
        read(xmlUnit,'(a)',iostat=iostat) xmlFileLine
        if (iostat == iostat_end) then
           call assertTrue(iExpectedLine .ge. (size(expected)), &
                &'Too few lines in XMLFile.')
           exit
        end if
        call assertEqual(iostat, 0, 'Unexpected XMLFile error.')
        iExpectedLine = iExpectedLine + 1
        call assertTrue(iExpectedLine .le. size(expected), &
             &'Too many lines in XMLFile.')
        if (iExpectedLine .le. size(expected)) then
           call assertEqual(expected(iExpectedLine),xmlFileLine, &
                & 'XML output file error.')
        end if
     end do
     close(xmlUnit, status='delete')

   end subroutine compareXMLFileToExpectation


end module Test_XmlPrinter_mod
