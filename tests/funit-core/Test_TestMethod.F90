!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_TestMethod
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
module Test_TestMethod
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('Test_TestMethod')
      call suite%addTest(TestMethod('testMethodWasRun', testMethodWasRun))

   end function suite

   subroutine testMethodWasRun()
      use PF_TestCase
      use PF_TestResult, only: TestResult, TestResult
      use PF_TestMethod, only: TestMethod, TestMethod
      use PF_Assert, only: assertEqual
      use PF_SerialContext
      use PF_Exception
      use PF_ExceptionList
      type (TestMethod) :: method
      type (TestResult) :: aResult

      method = TestMethod(name = 'testWasRun', method = testWasRun)
      aResult = TestResult()
      call method%run(aResult, SerialContext())
      call assertEqual(1, aResult%runCount())
      call assertEqual(1, aResult%failureCount())
   end subroutine testMethodWasRun

   subroutine testWasRun()
      use PF_ExceptionList, only: throw
      call throw('wasRun')
   end subroutine testWasRun

end module Test_TestMethod

