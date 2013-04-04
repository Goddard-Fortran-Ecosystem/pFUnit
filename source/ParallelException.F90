module ParallelException_mod
   use Exception_mod
   use ParallelContext_mod
   implicit none
   private

   public :: anyExceptions

   interface anyExceptions
      module procedure anyExceptions_context
   end interface anyExceptions

contains

   logical function anyExceptions_context(context) result(anyExcepts)
      class (ParallelContext) :: context

      logical, allocatable :: anyTable(:)

      allocate(anyTable(context%getNumProcesses()))

      call context%gather([anyExceptions()], anyTable)
      anyExcepts = any(anyTable)

   end function anyExceptions_context

end module ParallelException_mod
