module pf_TestAnnotation
   implicit none
   private

   public :: TestAnnotation

   type, abstract :: TestAnnotation
      private
   contains
      procedure(type_name), nopass, deferred :: type_name
   end type TestAnnotation

   abstract interface

      function type_name()
         character(:), allocatable :: type_name
      end function type_name

   end interface

end module pf_TestAnnotation


module pf_StringTestAnnotationMap
   use pf_TestAnnotation

#define Key __CHARACTER_DEFERRED
#define T TestAnnotation
#define T_polymorphic
#define Map StringTestAnnotationMap
#define MapIterator StringTestAnnotationMapIterator
#define Pair StringTestAnnotationPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef T_polymorphic
#undef Key

end module pf_StringTestAnnotationMap
