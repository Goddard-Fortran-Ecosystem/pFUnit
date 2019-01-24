!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_UnixProcess
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
module Test_UnixProcess
   use PF_TestSuite
   use PF_Assert
   use PF_ExceptionList
   use PF_UnixProcess
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('UnixProcess')
!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))

      call suite%addTest( &
           &   TestMethod('testIsActive', &
           &                  testIsActive))
      call suite%addTest( &
           &   TestMethod('testGetLine', &
           &                  testGetLine))
      call suite%addTest( &
           &   TestMethod('testGetLine2', &
           &                  testGetLine2))

   end function suite

   !------
   ! A bit self-referential, but at least it serves to drive
   ! development.Start a background command that persists.
   !------
   subroutine testIsActive()
      type (UnixProcess) :: process

      process = UnixProcess('sleep 10', runInBackground=.true.)
      call assertTrue(process%isActive(),'hmm')
      if (anyExceptions()) return

      call process%terminate()
      call assertFalse(process%isActive(),'huh')
      
   end subroutine testIsActive

   subroutine testGetLine()
      type (UnixProcess) :: process

      character(len=:), allocatable :: line

      process = UnixProcess('echo hello')
      line = process%getLine()
      call assertEqual('hello', trim(line))
      
   end subroutine testGetLine

   ! In this test, getLine is called twice because we first
   ! need to get the pid for the background process.
   subroutine testGetLine2()
      type (UnixProcess) :: process

      character(len=:), allocatable :: line

      process = UnixProcess('echo hello', runInBackground=.true.)
      line = process%getLine()

      call assertEqual('hello', trim(line))
      
   end subroutine testGetLine2

end module Test_UnixProcess
