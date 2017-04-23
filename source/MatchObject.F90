module PF_MatchObject_mod
   implicit none
   private

   public :: MatchObject

   type :: MatchObject
      logical :: found = .false.
      integer :: num_characters
      character(len=:), allocatable :: string
   end type MatchObject

end module PF_MatchObject_mod
