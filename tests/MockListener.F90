!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MockListener_mod
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 20 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 20 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module MockListener_mod
   use PF_TestListener_mod
   implicit none
   private

   public :: MockListener

   ! A pointer is used so that the TestListenerVector has a shallow
   ! copy of the log component.   This allows the test to access the results
   ! without adding otherwise unnecessary accessors.

   type, extends(TestListener) :: MockListener
     character(len=:), pointer :: log
   contains
     procedure :: addFailure
     procedure :: startTest
     procedure :: endTest
     procedure :: endRun
   end type MockListener

contains

  subroutine addFailure(this, testName, exceptions)
     use PF_ExceptionList_mod
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (ExceptionList), intent(in) :: exceptions

     write(this%log,'(a)') 'addFailure() was called'

  end subroutine addFailure

  subroutine startTest(this, testName)
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%log,'(a)') 'startTest() was called'

   end subroutine startTest

  subroutine endTest(this, testName)
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%log,'(a)') 'endTest() was called'

   end subroutine endTest

   subroutine endRun(this, result)
     use PF_AbstractTestResult_mod, only : AbstractTestResult
     class (MockListener), intent(inOut) :: this
     class (AbstractTestResult), intent(in) :: result
   end subroutine endRun


end module MockListener_mod
