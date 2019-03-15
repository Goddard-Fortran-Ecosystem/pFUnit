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
           &   TestMethod('testIs_active', &
           &                  testIs_active))
      call suite%addTest( &
           &   TestMethod('testGet_line', &
           &                  testGet_line))
      call suite%addTest( &
           &   TestMethod('testGet_Line2', &
           &                  testGet_line2))

   end function suite

   !------
   ! A bit self-referential, but at least it serves to drive
   ! development.Start a background command that persists.
   !------
   subroutine testIs_active()
      type (UnixProcess) :: process

      process = UnixProcess('sleep 10', runInBackground=.true.)
      call assertTrue(process%is_active(),'hmm')
      if (anyExceptions()) return

      call process%terminate()
      call assertFalse(process%is_active(),'huh')
      
   end subroutine testIs_active

   subroutine testGet_Line()
      type (UnixProcess) :: process

      character(len=:), allocatable :: line

      process = UnixProcess('echo hello')
      line = process%get_line()
      call assertEqual('hello', trim(line))
      
    end subroutine testGet_Line

   ! In this test, getLine is called twice because we first
   ! need to get the pid for the background process.
   subroutine testGet_Line2()
      type (UnixProcess) :: process

      character(len=:), allocatable :: line

      process = UnixProcess('echo hello', runInBackground=.true.)
      line = process%get_line()

      call assertEqual('hello', trim(line))
      
    end subroutine testGet_Line2

end module Test_UnixProcess
