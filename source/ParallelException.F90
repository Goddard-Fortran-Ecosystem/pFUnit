module ParallelException_mod
   use ParallelContext_mod
   use Exception_mod
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

   logical function anyExceptions_context(context) result(anyExcepts)
      class (ParallelContext) :: context

      logical, allocatable :: anyTable(:)

      allocate(anyTable(context%getNumProcesses()))

      call context%gather([anyExceptions()], anyTable)
      anyExcepts = any(anyTable)

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

      type (ExceptionList) :: globalList
      type (ExceptionList) :: localList
      integer :: globalExceptionCount
      character(len=MAXLEN_MESSAGE) :: msg
      integer :: i

      integer :: totalExceptions

      totalExceptions = getNumExceptions(context)
      if (totalExceptions > 0) then

         if (context%isRootProcess()) then
            allocate(globalList%exceptions(globalExceptionCount))
         else
            allocate(globalList%exceptions(1)) ! mpi does not like 0-sized arrays
         end if
         allocate(localList%exceptions(getNumExceptions()))

         do i = 1, getNumExceptions()
            localList%exceptions(i) = catchAny() ! drains singleton exception list on all PEs
            call context%labelProcess(localList%exceptions(i)%message)
         end do

         call context%gather(localList%exceptions(:)%nullFlag, globalList%exceptions(:)%nullFlag)
         call context%gather(localList%exceptions(:)%fileName, globalList%exceptions(:)%fileName)
         call context%gather(localList%exceptions(:)%lineNumber, globalList%exceptions(:)%lineNumber)
         call context%gather(localList%exceptions(:)%message, globalList%exceptions(:)%message)
      
         if (context%isRootProcess()) then ! rethrow
            do i = 1, totalExceptions
               associate(e => globalList%exceptions(i))
                 call throw(e%message, e%lineNumber, e%fileName)
               end associate
            end do
         end if

      end if

   end subroutine gather

end module ParallelException_mod
