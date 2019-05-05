module PF_LiteralPattern
   use PF_AbstractPattern
   use PF_MatchObject
   implicit none
   private

   public :: LiteralPattern

   type, extends(AbstractPattern) :: LiteralPattern
      private
      character(len=:), allocatable :: literal
   contains
      procedure :: match
   end type LiteralPattern

   interface LiteralPattern
      module procedure new_LiteralPattern
   end interface LiteralPattern

contains


   function new_LiteralPattern(literal) result(pattern)
      type (LiteralPattern) :: pattern
      
      character(len=*), intent(in) :: literal

      pattern%literal = literal
      
   end function new_LiteralPattern



   function match(this, string)
      type (MatchObject) :: match
      class (LiteralPattern), intent(in) :: this
      character(len=*), intent(in) :: string

      integer :: idx

      idx = index(string, this%literal)

      if (idx == 1) then
         match%found = .true.
         match%num_characters = len(this%literal)
         match%string = this%literal
      else
         match%found = .false.
         match%num_characters = 0
         match%string = ''
      end if

   end function match

end module PF_LiteralPattern
