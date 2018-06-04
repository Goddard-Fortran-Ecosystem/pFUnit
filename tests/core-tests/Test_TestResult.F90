!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_TestResult_mod
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
module Test_TestResult_mod
   use PF_TestCase_mod
   use PF_TestResult_mod, only: TestResult
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite_mod, only: TestSuite
      use PF_TestCase_mod
      use PF_TestMethod_mod, only: TestMethod
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
      use PF_Assert_mod, only: assertEqual
      use PF_TestResult_mod, only: TestResult
      use PF_TestCase_mod
      use SimpleTestCase_mod
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
      use PF_Assert_mod, only: assertEqual
      use PF_Exception_mod, only: Exception
      use PF_ExceptionList_mod, only: ExceptionList
      use SimpleTestCase_mod, only: SimpleTestCase
      use PF_SurrogateTestCase_mod
      use PF_TestCase_mod

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
      use PF_TestListener_mod
      use PF_SurrogateTestCase_mod
      use MockListener_mod
      use PF_Assert_mod
      use SimpleTestCase_mod
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
      use PF_TestListener_mod
      use MockListener_mod
      use PF_Assert_mod
      use SimpleTestCase_mod
      use PF_SurrogateTestCase_mod
      use PF_TestCase_mod

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
      use PF_TestListener_mod
      use MockListener_mod
      use PF_Assert_mod
      use PF_Exception_mod
      use PF_ExceptionList_mod
      use SimpleTestCase_mod
      use PF_SurrogateTestCase_mod
      use PF_TestCase_mod
      
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

end module Test_TestResult_mod
