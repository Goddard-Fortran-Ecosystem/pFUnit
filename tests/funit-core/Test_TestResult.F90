!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_TestResult
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 21 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 21 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module Test_TestResult
   use PF_TestCase
   use PF_TestResult, only: TestResult
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite
      use PF_TestCase
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('TestResultSuite')

!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))


      call suite%addTest( &
           &   TestMethod('testGetNumRun', &
           &                  testGetNumRun))
      call suite%addTest( &
           &   TestMethod('testGetNumFailed', &
           &                  testGetNumFailed))

      call suite%addTest( &
           &   TestMethod('testAddListenerStart', &
           &                  testAddListenerStart))
      call suite%addTest( &
           &   TestMethod('testAddListenerEnd', &
           &                  testAddListenerEnd))
      call suite%addTest( &
           &   TestMethod('testAddListenerFailure', &
           &                  testAddListenerFailure))

   end function suite

   subroutine testGetNumRun()
      use PF_Assert, only: assertEqual
      use PF_TestResult, only: TestResult
      use PF_TestCase
      use pf_SimpleTestCase
      type (TestResult) :: aResult
      type (SimpleTestCase) :: tstCase

      aResult = TestResult()
      call assertEqual(0, aResult%runCount())

      tstCase = newSimpleTestCase('method1', method1)

      call aResult%startTest(tstCase%getSurrogate())
      call aResult%endTest(tstCase%getSurrogate())
      call assertEqual(1, aResult%runCount())

      call aResult%startTest(tstCase%getSurrogate())
      call aResult%endTest(tstCase%getSurrogate())
      call assertEqual(2, aResult%runCount())

   end subroutine testGetNumRun

   subroutine testGetNumFailed()
      use PF_Assert, only: assertEqual
      use PF_Exception, only: Exception
      use PF_ExceptionList, only: ExceptionList
      use pf_SimpleTestCase, only: SimpleTestCase
      use PF_SurrogateTestCase
      use PF_TestCase

      type (TestResult) :: aResult
      type (ExceptionList) :: list
      type (SimpleTestCase) :: aTest

      call aTest%setName('foo')
      call aTest%setSurrogate()
      aResult = TestResult()
      call assertEqual(0, aResult%failureCount())

      call list%push_back(Exception('fail'))
      call aResult%addFailure(aTest%getSurrogate(), list)
      call assertEqual(1, aResult%failureCount())

      call list%clear()
      call list%push_back(Exception('fail again'))
      call aResult%addFailure(aTest%getSurrogate(), list)
      call assertEqual(2, aResult%failureCount())

   end subroutine testGetNumFailed

   subroutine testAddListenerStart()
      use PF_TestListener
      use PF_SurrogateTestCase
      use pf_MockListener
      use PF_Assert
      use pf_SimpleTestCase
      type (TestResult) :: result
      type (MockListener), target :: listener
      
      type (SimpleTestCase) :: tstCase
      character(40), target :: buffer

      result = TestResult()

      listener%log => buffer
      call result%addListener(listener)

      tstCase = newSimpleTestCase('method1', method1)
      call result%startTest(tstCase%getSurrogate())
      call assertEqual('startTest() was called', trim(listener%log))

   end subroutine testAddListenerStart

   subroutine testAddListenerEnd()
      use PF_TestListener
      use pf_MockListener
      use PF_Assert
      use pf_SimpleTestCase
      use PF_SurrogateTestCase
      use PF_TestCase

      type (TestResult) :: result
      type (MockListener), target :: listener
      type (SimpleTestCase) :: tstCase
      character(40), target :: buffer
      
      result = TestResult()
      listener%log => buffer
      call result%addListener(listener)
      tstCase = newSimpleTestCase('method1', method1)
      call result%endTest(tstCase%getSurrogate())
      call assertEqual('endTest() was called', listener%log)

   end subroutine testAddListenerEnd

   subroutine testAddListenerFailure()
      use PF_TestListener
      use pf_MockListener
      use PF_Assert
      use PF_Exception
      use PF_ExceptionList
      use pf_SimpleTestCase
      use PF_SurrogateTestCase
      use PF_TestCase
      
      type (TestResult) :: result
      type (MockListener), target :: listener
      type (Exception) :: anException
      
      class(TestCase), allocatable :: tstCase
      type (ExceptionList) :: list
      character(40), target :: buffer

      result = TestResult()
      listener%log => buffer
      call result%addListener(listener)
      allocate(tstCase, source = newSimpleTestCase('method1', method1))
      call list%push_back(anException)
      call result%addFailure(tstCase%getSurrogate(), list)
      call assertEqual('addFailure() was called', listener%log)


   end subroutine testAddListenerFailure

end module Test_TestResult
