! Default implementation is for a serial process
! Subclass for MPI operations
module ParallelContext_mod
   implicit none
   private

   public :: ParallelContext

   type, abstract :: ParallelContext
   contains
      procedure :: isActive
      procedure :: isRootProcess
      procedure(I_numProcesses), deferred :: getNumProcesses
      procedure(I_processRank), deferred :: processRank
      procedure(I_sum), deferred :: sum
      generic :: gather => gatherString
      generic :: gather => gatherInteger
      generic :: gather => gatherLogical
      procedure(I_gatherString), deferred :: gatherString
      procedure(I_gatherInteger), deferred :: gatherInteger
      procedure(I_gatherLogical), deferred :: gatherLogical
   end type ParallelContext

   abstract interface
      integer function I_numProcesses(this)
         import ParallelContext
         class(ParallelContext), intent(in) :: this
      end function I_numProcesses

      integer function I_processRank(this)
         import ParallelContext
         class(ParallelContext), intent(in) :: this
      end function I_processRank

      integer function I_sum(this, value)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         integer, intent(in) :: value
      end function I_sum

      subroutine I_GatherString(this, values, list)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         character(len=*), intent(in) :: values(:)
         character(len=*), intent(out) :: list(:)
      end subroutine I_GatherString

      subroutine I_GatherInteger(this, values, list)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         integer, intent(in) :: values(:)
         integer, intent(out) :: list(:)
      end subroutine I_GatherInteger

      subroutine I_GatherLogical(this, values, list)
         import ParallelContext
         class (ParallelContext), intent(in) :: this
         logical, intent(in) :: values(:)
         logical, intent(out) :: list(:)
      end subroutine I_GatherLogical

   end interface

contains


   logical function isActive(this)
      class (ParallelContext),  intent(in) :: this
      isActive = .true.
   end function isActive

   subroutine barrier(this)
      class (ParallelContext), intent(in) :: this
   end subroutine barrier

   logical function isRootProcess(this)
      class (ParallelContext), intent(in) :: this
      isRootProcess = .true.
   end function isRootProcess

end module ParallelContext_mod
