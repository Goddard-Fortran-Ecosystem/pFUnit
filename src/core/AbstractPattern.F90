module PF_AbstractPattern_mod
   implicit none
   private
 
   public :: AbstractPattern

   type, abstract :: AbstractPattern
   contains
      procedure(match), deferred :: match
   end type AbstractPattern

   abstract interface

      function match(this, string)
         use PF_MatchObject_mod
         import AbstractPattern
         implicit none
         type (MatchObject) :: match
         class (AbstractPattern), intent(in) :: this
         character(len=*), intent(in) :: string
      end function match
   end interface

end module PF_AbstractPattern_mod

module PF_AbstractPatternVector_mod
   use PF_AbstractPattern_mod
#define _type class (AbstractPattern)
#define _allocatable
#define _vector AbstractPatternVector
#define _iterator AbstractPatternVectorIterator

#include "templates/vector.inc"

end module PF_AbstractPatternVector_mod
