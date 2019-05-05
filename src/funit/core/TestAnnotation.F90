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

#define _map StringTestAnnotationMap
#define _iterator StringTestAnnotationMapIterator
#include "types/key_deferredLengthString.inc"
#define _value_allocatable
#define _value class(TestAnnotation)
#define _alt
#include "templates/map.inc"

end module pf_StringTestAnnotationMap
