module PF_RepeatPattern
   use PF_AbstractPattern
   use PF_MatchObject
   implicit none
   private

   public :: RepeatPattern

   type, extends(AbstractPattern) :: RepeatPattern
      private
      class (AbstractPattern), allocatable :: base
   contains
      procedure :: match
   end type RepeatPattern

   interface RepeatPattern
      module procedure new_RepeatPattern
   end interface RepeatPattern

contains


   function new_RepeatPattern(base) result(pattern)
      class (AbstractPattern), intent(in) :: base
      type (RepeatPattern) :: pattern

      pattern%base = base

   end function new_RepeatPattern


   function match(this, string)
      type (MatchObject) :: match
      class (RepeatPattern), intent(in) :: this
      character(len=*), intent(in) :: string

      integer :: i
      type (MatchObject) :: m

      match%found = .true. ! always
      match%num_characters = 0
      match%string = ''
      
      i = 0
      do
         m = this%base%match(string(i+1:))
         if (m%found) then
            i = i + m%num_characters
            match%num_characters = match%num_characters + m%num_characters
            match%string = match%string // m%string
         else
            exit
         end if
      end do


   end function match

end module PF_RepeatPattern
