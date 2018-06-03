!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_TestMethod_mod
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
module Test_TestMethod_mod
   use PF_TestSuite_mod, only: TestSuite
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite_mod, only: TestSuite
      use PF_TestMethod_mod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('Test_TestMethod')
      call suite%addTest(TestMethod('testMethodWasRun', testMethodWasRun))

   end function suite

   subroutine testMethodWasRun()
      use PF_TestCase_mod
      use PF_TestResult_mod, only: TestResult, TestResult
      use PF_TestMethod_mod, only: TestMethod, TestMethod
      use PF_Assert_mod, only: assertEqual
      use PF_SerialContext_mod
      use PF_Exception_mod
      use PF_ExceptionList_mod
      type (TestMethod) :: method
      type (TestResult) :: aResult

      method = TestMethod(name = 'testWasRun', method = testWasRun)
      aResult = TestResult()
      call method%run(aResult, SerialContext())
      call assertEqual(1, aResult%runCount())
      call assertEqual(1, aResult%failureCount())
   end subroutine testMethodWasRun

   subroutine testWasRun()
      use PF_ExceptionList_mod, only: throw
      call throw('wasRun')
   end subroutine testWasRun

end module Test_TestMethod_mod

