!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_FixtureTestCase_mod
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
module Test_FixtureTestCase_mod
   use PF_TestSuite_mod
   use PF_TestResult_mod, only: TestResult
   implicit none
   private

   public :: suite

contains

   function suite() result(aSuite)
      use PF_TestSuite_mod, only: TestSuite
      use PF_TestMethod_mod, only: TestMethod
      type (TestSuite) :: aSuite

      aSuite = TestSuite('Test_FixtureTestCase')

!#define ADD(method) call aSuite%addTest(TestMethod(REFLECT(method)))

      call aSuite%addTest( &
           &   TestMethod('testRunWithFixture', &
           &                  testRunWithFixture))
      call aSuite%addTest( &
           &   TestMethod('testBrokenTestCase', &
           &                  testBrokenTestCase))
      call aSuite%addTest( &
           &   TestMethod('testBrokenSetUpCase', &
           &                  testBrokenSetUpCase))

   end function suite

   subroutine testRunWithFixture()
      use PF_TestCase_mod
      use FixtureTestCase_mod, only: FixtureTestCase, newFixtureTestCase
      use FixtureTestCase_mod, only: delete
      use PF_SerialContext_mod
      use PF_Assert_mod, only: assertEqual
      type (FixtureTestCase) :: aTest
      type (TestResult) :: aTestResult

      aTestResult = TestResult()
      aTest = newFixtureTestCase()
      call aTest%setSurrogate()
      call aTest%run(aTestResult, SerialContext())
      call assertEqual('setUp run tearDown', aTest%runLog)
      call delete(aTest)

   end subroutine testRunWithFixture

   subroutine testBrokenTestCase()
      use PF_TestCase_mod
      use BrokenTestCase_mod, only: BrokenTestCase
      use PF_Assert_mod, only: assertEqual
      use PF_SerialContext_mod
      type (BrokenTestCase) :: test
      type (TestResult) :: aTestResult

      call test%setSurrogate()
      call test%setName('foo')
      aTestResult = TestResult()

      call test%run(aTestResult, SerialContext())
      call assertEqual('setUp broken run tearDown', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenTestCase

   subroutine testBrokenSetUpCase()
      use PF_TestCase_mod
      use BrokenSetUpCase_mod, only: BrokenSetUpCase
      use PF_Assert_mod, only: assertEqual
      use PF_SerialContext_mod
      type (BrokenSetUpCase) :: test
      type (TestResult) :: aTestResult

      call test%setSurrogate()
      call test%setName('foo')
      aTestResult = TestResult()
      call test%run(aTestResult, SerialContext())
      call assertEqual('broken setUp', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenSetUpCase

end module Test_FixtureTestCase_mod

