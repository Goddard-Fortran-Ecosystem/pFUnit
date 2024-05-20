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

#define Map StringTestAnnotationMap
#define MapIterator StringTestAnnotationMapIterator
#define Pair StringTestAnnotationPair
#define Key __CHARACTER_DEFERRED
#define T TestAnnotation
#define T_polymorphic
#include "map/template.inc"

end module pf_StringTestAnnotationMap
