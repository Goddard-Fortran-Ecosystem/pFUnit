#include "unused_dummy.fh"

module PF_DotPattern
   use PF_AbstractPattern
   use PF_MatchObject
   implicit none
   private

   public :: DotPattern

   type, extends(AbstractPattern) :: DotPattern
      private
   contains
      procedure :: match
   end type DotPattern

   interface DotPattern
      module procedure new_DotPattern
   end interface DotPattern

contains


   function new_DotPattern() result(pattern)
      type (DotPattern) :: pattern
      _UNUSED_DUMMY(pattern)
   end function new_DotPattern


   function match(this, string)
      type (MatchObject) :: match
      class (DotPattern), intent(in) :: this
      character(len=*), intent(in) :: string

      _UNUSED_DUMMY(this)

      if (len(string) > 0) then
         match%found = .true.
         match%num_characters = 1
         match%string = string(1:1)
      else
         match%found = .false.
         match%num_characters = 0
         match%string = ''
      end if

   end function match

end module PF_DotPattern
