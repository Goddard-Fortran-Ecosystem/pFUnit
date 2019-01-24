#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MockListener
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
module pf_MockListener
   use PF_TestListener
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
     procedure :: disableTest
     procedure :: endTest
     procedure :: endRun
     procedure :: addSuccess
   end type MockListener

contains

  subroutine addFailure(this, testName, exceptions)
     use PF_ExceptionList
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (ExceptionList), intent(in) :: exceptions

     _UNUSED_DUMMY(testName)
     _UNUSED_DUMMY(exceptions)

     write(this%log,'(a)') 'addFailure() was called'

  end subroutine addFailure

  subroutine startTest(this, testName)
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     _UNUSED_DUMMY(testName)

     write(this%log,'(a)') 'startTest() was called'

   end subroutine startTest

  subroutine disableTest(this, testName)
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     _UNUSED_DUMMY(testName)

     write(this%log,'(a)') 'disableTest() was called'

  end subroutine disableTest

  subroutine endTest(this, testName)
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     _UNUSED_DUMMY(testName)

     write(this%log,'(a)') 'endTest() was called'

   end subroutine endTest


   subroutine endRun(this, result)
     use PF_AbstractTestResult, only : AbstractTestResult
     class (MockListener), intent(inOut) :: this
     class (AbstractTestResult), intent(in) :: result

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(result)

   end subroutine endRun


   subroutine addSuccess(this, testName)
      class (MockListener), intent(inout) :: this
      character(*), intent(in) :: testName

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)

   end subroutine addSuccess


end module Pf_MockListener
