!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: ParallelException
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
module PF_ParallelException_mod
   use PF_ParallelContext_mod
   use PF_Exception_mod
   implicit none
   private

   public :: anyExceptions
   public :: getNumExceptions
   public :: gather

   interface anyExceptions
      module procedure anyExceptions_context
   end interface anyExceptions

   interface getNumExceptions
      module procedure getNumExceptions_context
   end interface getNumExceptions

contains

   logical function anyExceptions_context(context) result(anyExcept)
      class (ParallelContext) :: context

!      logical, allocatable :: anyTable(:)

      anyExcept = context%allReduce(anyExceptions())
   end function anyExceptions_context

   integer function getNumExceptions_context(context) result(numExceptions)
      class (ParallelContext) :: context

      integer, allocatable :: counts(:)

      allocate(counts(context%getNumProcesses()))
      call context%gather([getNumExceptions()], counts)
      numExceptions = sum(counts)

   end function getNumExceptions_context

   subroutine gather(context)
      class (ParallelContext), intent(in) :: context

      type (ExceptionList) :: list
      integer :: i

      integer :: totalExceptions, n

      totalExceptions = getNumExceptions(context)
      if (totalExceptions > 0) then

         allocate(list%exceptions(getNumExceptions()))

         n = getNumExceptions()
         do i = 1, n
            list%exceptions(i) = catchNext() ! drains singleton exception list on all PEs
!!$            call context%labelProcess(list%exceptions(i)%message)
         end do

         call list%gather(context)
         
         if (context%isRootProcess()) then ! rethrow
            do i = 1, totalExceptions
               associate(e => list%exceptions(i))
                 call throw(e%message, e%location)
               end associate
            end do
         end if

         deallocate(list%exceptions)

      end if

   end subroutine gather

end module PF_ParallelException_mod
