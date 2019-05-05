!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_FixtureTestCase
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
module Test_FixtureTestCase
   use PF_TestSuite
   use PF_TestResult, only: TestResult
   implicit none
   private

   public :: suite

contains

   function suite() result(aSuite)
      use PF_TestSuite, only: TestSuite
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: aSuite

      aSuite = TestSuite('Test_FixtureTestCase')

!#define ADD(method) call aSuite%addTest(TestMethod(REFLECT(method)))

      call aSuite%addTest( &
           &   TestMethod('testRunWithFixture', &
           &                  testRunWithFixture))
      call aSuite%addTest( &
           &   TestMethod('testBrokenTest', &
           &                  testBrokenTest))
      call aSuite%addTest( &
           &   TestMethod('testBrokenSetUp', &
           &                  testBrokenSetUp))

   end function suite

   subroutine testRunWithFixture()
      use PF_TestCase
      use FixtureTestCase, only: FixtureTest, newFixtureTest
      use FixtureTestCase, only: delete
      use PF_SerialContext
      use PF_Assert, only: assertEqual
      type (FixtureTest) :: aTest
      type (TestResult) :: aTestResult

      aTestResult = TestResult()
      aTest = newFixtureTest()
      call aTest%setSurrogate()
      call aTest%run(aTestResult, SerialContext())
      call assertEqual('setUp run tearDown', aTest%runLog)
      call delete(aTest)

   end subroutine testRunWithFixture

   subroutine testBrokenTest()
      use PF_TestCase
      use BrokenTestCase, only: BrokenTest
      use PF_Assert, only: assertEqual
      use PF_SerialContext
      type (BrokenTest) :: test
      type (TestResult) :: aTestResult

      call test%setSurrogate()
      call test%setName('foo')
      aTestResult = TestResult()

      call test%run(aTestResult, SerialContext())
      call assertEqual('setUp broken run tearDown', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenTest

   subroutine testBrokenSetUp()
      use PF_TestCase
      use BrokenSetUpCase, only: BrokenSetUp
      use PF_Assert, only: assertEqual
      use PF_SerialContext
      type (BrokenSetUp) :: test
      type (TestResult) :: aTestResult

      call test%setSurrogate()
      call test%setName('foo')
      aTestResult = TestResult()
      call test%run(aTestResult, SerialContext())
      call assertEqual('broken setUp', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenSetUp

end module Test_FixtureTestCase

