!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: ParallelContext
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
! Default implementation is for a serial process
! Subclass for MPI operations

#include "unused_dummy.fh"
module PF_ParallelContext
   implicit none
   private

   public :: ParallelContext

   type, abstract :: ParallelContext
   contains
      procedure :: isActive
      procedure :: isRootProcess
      procedure(getNumProcesses), deferred :: getNumProcesses
      procedure(processRank), deferred :: processRank
      procedure(reduce), deferred :: sum_global
      generic :: sum => sum_global
      procedure(reduce), deferred :: maximum
      generic :: gather => gatherString
!      generic :: gather => gatherInteger
      generic :: gather => gatherLogical
      procedure(gatherString), deferred :: gatherString
      procedure(gatherInteger), deferred :: gatherInteger
      procedure(gatherLogical), deferred :: gatherLogical
      procedure :: labelProcess
      procedure :: barrier
      procedure(allReduceLogical), deferred :: allReduce
   end type ParallelContext

   abstract interface
      integer function getNumProcesses(this)
         import ParallelContext
         class(ParallelContext), intent(in) :: this
      end function getNumProcesses

      integer function processRank(this)
         import ParallelContext
         class(ParallelContext), intent(in) :: this
      end function processRank

      integer function reduce(this, value)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         integer, intent(in) :: value
       end function reduce
       
      subroutine gatherString(this, values, list)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         character(len=*), intent(in) :: values(:)
         character(len=*), intent(out) :: list(:)
      end subroutine gatherString

      function gatherInteger(this, values) result(global_list)
         import ParallelContext
         integer, allocatable :: global_list(:)
         class (ParallelContext), intent(in) :: this
         integer, intent(in) :: values(:)
      end function gatherInteger

      subroutine gatherLogical(this, values, list)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         logical, intent(in) :: values(:)
         logical, intent(out) :: list(:)
      end subroutine gatherLogical

      logical function allReduceLogical(this, q) result(anyQ)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         logical, intent(in) :: q
      end function allReduceLogical

   end interface

contains

   logical function isActive(this)
      class (ParallelContext),  intent(in) :: this
      _UNUSED_DUMMY(this)
      isActive = .true.
   end function isActive

   subroutine barrier(this)
      class (ParallelContext), intent(in) :: this
      _UNUSED_DUMMY(this)
   end subroutine barrier

   logical function isRootProcess(this)
      class (ParallelContext), intent(in) :: this
      _UNUSED_DUMMY(this)
      isRootProcess = .true.
   end function isRootProcess

   ! Default is to return a copy of the message.
   function labelProcess(this, message) result(labelled_message)
      character(len=:), allocatable :: labelled_message
      class (ParallelContext), intent(in) :: this
      character(*), intent(in) :: message
      _UNUSED_DUMMY(this)

      labelled_message = message

   end function labelProcess

end module PF_ParallelContext
