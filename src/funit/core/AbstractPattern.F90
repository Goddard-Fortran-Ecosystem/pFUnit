module PF_AbstractPattern
   implicit none
   private
 
   public :: AbstractPattern

   type, abstract :: AbstractPattern
   contains
      procedure(match), deferred :: match
   end type AbstractPattern

   abstract interface

      function match(this, string)
         use PF_MatchObject
         import AbstractPattern
         implicit none
         type (MatchObject) :: match
         class (AbstractPattern), intent(in) :: this
         character(len=*), intent(in) :: string
      end function match
   end interface

end module PF_AbstractPattern

module PF_AbstractPatternVector
   use PF_AbstractPattern
#define _type class (AbstractPattern)
#define _allocatable
#define _vector AbstractPatternVector
#define _iterator AbstractPatternVectorIterator

#include "templates/vector.inc"

end module PF_AbstractPatternVector
