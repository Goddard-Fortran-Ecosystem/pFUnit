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
   use PF_TestSuite_mod, only: TestSuite, newTestSuite
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite_mod, only: TestSuite, newTestSuite
      use PF_TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('Test_TestMethod')
      call suite%addTest(newTestMethod('testMethodWasRun', testMethodWasRun))

   end function suite

   subroutine testMethodWasRun()
      use PF_TestCase_mod
      use PF_TestResult_mod, only: TestResult, newTestResult
      use PF_TestMethod_mod, only: TestMethod, newTestMethod
      use PF_Assert_mod, only: assertEqual
      use PF_SerialContext_mod
      use PF_Exception_mod
      use PF_ExceptionList_mod
      type (TestMethod) :: method
      type (TestResult) :: aResult

      method = newTestMethod(name = 'testWasRun', method = testWasRun)
      aResult = newTestResult()
      call method%run(aResult, newSerialContext())
      print*,__FILE__,__LINE__,getNumExceptions()
      print*,__FILE__,__LINE__, aResult%runCount()
      call assertEqual(1, aResult%runCount())
      print*,__FILE__,__LINE__, aResult%failureCount()
      call assertEqual(1, aResult%failureCount())
      print*,__FILE__,__LINE__,getNumExceptions()
   end subroutine testMethodWasRun

   subroutine testWasRun()
      use PF_ExceptionList_mod, only: throw
      use PF_ExceptionList_mod, only: getNumExceptions
      call throw('wasRun')
   end subroutine testWasRun

end module Test_TestMethod_mod

