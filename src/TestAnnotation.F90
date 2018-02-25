module pf_TestAnnotation_mod
   implicit none
   private

   public :: TestAnnotation
   public :: Ignore ! singleton

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

   type, extends(TestAnnotation) :: IgnoreAnnotation
   contains
      procedure, nopass :: type_name => ignore_type_name
   end type IgnoreAnnotation

   ! Instance is public, type is private; semi-singleton
   type (IgnoreAnnotation) :: Ignore

contains


   function ignore_type_name() result(type_name)
      character(:), allocatable :: type_name

      type_name = 'Ignore'
      
   end function ignore_type_name

end module pf_TestAnnotation_mod


module pf_StringTestAnnotationMap_mod
   use pf_TestAnnotation_mod

#define _map StringTestAnnotationMap
#define _iterator StringTestAnnotationMapIterator
#include "types/key_deferredLengthString.inc"
#define _value_allocatable
#define _value class(TestAnnotation)
#define _alt
#include "templates/map.inc"

end module pf_StringTestAnnotationMap_mod
