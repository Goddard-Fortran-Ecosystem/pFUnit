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
#define T AbstractPattern
#define T_polymorphic
#define Vector AbstractPatternVector
#define VectorIterator AbstractPatternVectorIterator

#include "vector/template.inc"

end module PF_AbstractPatternVector
