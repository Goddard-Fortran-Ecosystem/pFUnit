#include "unused_dummy.fh"
!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TestListener
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module PF_TestListener
   implicit none
   private

   public :: TestListener
   public :: ListenerPointer

   type, abstract :: TestListener
      private
      logical :: useDebug = .false.
   contains
     procedure(addFailure), deferred :: addFailure
     procedure(startTest), deferred :: startTest
     procedure(startTest), deferred :: addSuccess
     procedure(endTest), deferred :: endTest
!     procedure(startRun), deferred :: startRun  ! make deferred when ready
     procedure :: disableTest
     procedure :: addError
     procedure :: setDebug
     procedure :: debug
   end type TestListener

   type ListenerPointer
     class (TestListener), pointer :: pListener
   end type ListenerPointer

   abstract interface
      subroutine addFailure(this, testName, exceptions)
         use PF_Exception
         use PF_ExceptionList
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
         type (ExceptionList), intent(in) :: exceptions
      end subroutine addFailure

      subroutine startTest(this, testName)
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
      end subroutine startTest
    
      subroutine endTest(this, testName)
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
      end subroutine endTest

   end interface

contains

   ! Most scenarios in Fortran cannot diagnose true errors, so
   ! an empty stub is provided here for convenience.
   subroutine addError(this, testName, exceptions)
      use PF_ExceptionList
      class (TestListener), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
      _UNUSED_DUMMY(exceptions)
   end subroutine addError

   ! Promoted from BaseTestRunner.F90. Every listener can have debug
   ! behaviors.
    subroutine setDebug(this)
       class (TestListener), intent(inout) :: this
       this%useDebug = .true.
    end subroutine setDebug

    logical function debug(this)
       class (TestListener), intent(inout) :: this
       debug = this%useDebug
    end function debug

    ! By default, disableTest() does nothing.
    ! Maybe some listeners don't care.
    subroutine disableTest(this, testName)
       class (TestListener), intent(inout) :: this
       character(len=*), intent(in) :: testName
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
    end subroutine disableTest
    

 end module PF_TestListener
