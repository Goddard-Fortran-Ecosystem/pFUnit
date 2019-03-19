#include "unused_dummy.fh"
!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: SerialContext
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
module PF_SerialContext
   use PF_ParallelContext
   implicit none
   private

   public :: SerialContext
   public :: THE_SERIAL_CONTEXT

   type, extends(ParallelContext) :: SerialContext
   contains
      procedure :: getNumProcesses
      procedure :: processRank
      procedure :: sum_global
      procedure :: maximum
      procedure :: gatherString
      procedure :: gatherInteger
      procedure :: gatherLogical
      procedure :: allReduce
!TODO - NAG does not yet support FINAL keyword
!!$$      final :: clean
   end type SerialContext

   ! Really a parameter
   type (SerialContext), protected :: THE_SERIAL_CONTEXT

   interface SerialContext
      module procedure new_SerialContext
   end interface SerialContext

contains

   function new_SerialContext() result(context)
      type (SerialContext) :: context
      ! prevent NAG warning about unassigned return value.
      context = THE_SERIAL_CONTEXT
   end function new_SerialContext

   integer function getNumProcesses(this)
      class (SerialContext),  intent(in) :: this

      _UNUSED_DUMMY(this)
      getNumProcesses = 1

   end function getNumProcesses

   integer function processRank(this)
      class (SerialContext),  intent(in) :: this

      _UNUSED_DUMMY(this)
      processRank = 0

   end function processRank

   integer function sum_global(this, value)
      class (SerialContext), intent(in) :: this
      integer, intent(in) :: value

      _UNUSED_DUMMY(this)
      sum_global = value

   end function sum_global


   integer function maximum(this, value)
      class (SerialContext), intent(in) :: this
      integer, intent(in) :: value

      _UNUSED_DUMMY(this)
      maximum = value

   end function maximum


   subroutine gatherString(this, values, list)
      class (SerialContext), intent(in) :: this
      character(len=*), intent(in) :: values(:)
      character(len=*), intent(out) :: list(:)

      _UNUSED_DUMMY(this)
      list = values

   end subroutine gatherString


   function gatherInteger(this, values) result(global_list)
      integer, allocatable :: global_list(:)
      class (SerialContext), intent(in) :: this
      integer, intent(in) :: values(:)

      _UNUSED_DUMMY(this)
      global_list = values

   end function gatherInteger


   subroutine gatherLogical(this, values, list)
      class (SerialContext), intent(in) :: this
      logical, intent(in) :: values(:)
      logical, intent(out) :: list(:)

      _UNUSED_DUMMY(this)
      list = values
   end subroutine gatherLogical



   logical function allReduce(this, q) result(anyQ)
      class (SerialContext), intent(in) :: this
      logical, intent(in) :: q

      _UNUSED_DUMMY(this)
      anyQ = q
   end function allReduce

   subroutine clean(this)
      type (SerialContext), intent(inout) :: this
      _UNUSED_DUMMY(this)
   end subroutine clean

end module PF_SerialContext
