! This module converts integers/real's to strings of a specific format
! for unit test diagnostics.  Basically just a wrapper for Fortran
! formatting, but functional programming provides a better style in many
! situations.
! 
! Further control of field width could be added at a later time.
!

module StringUtilities
   implicit none
   private
   
   public :: toString
   public :: MAXLEN_REAL_STRING


   integer, parameter :: MAXLEN_REAL_STRING = 25
   interface toString
      module Procedure toString_realScalar
      module Procedure toString_integerScalar
      module Procedure toString_integer1D
   end interface

contains

   character(len=MAXLEN_REAL_STRING) function toString_realScalar(value) result(buffer)
      real, intent(in) :: value

      write(buffer,'(SP,G14.7)') value
      buffer = adjustL(buffer)

   end function toString_realScalar

   character(len=MAXLEN_REAL_STRING) function toString_integerScalar(value) result(buffer)
      integer, intent(in) :: value
      character(len=20) :: fmt

      fmt = '(I0)'
      write(buffer,trim(fmt)) value
      buffer = adjustL(buffer)

   end function toString_integerScalar

   character(len=MAXLEN_REAL_STRING) function toString_integer1D(values) result(buffer)
      integer, intent(in) :: values(:)
      character(len=20) :: fmt

      if (size(values) > 1) then
         write(fmt,'("(I0,",I0,a)') size(values) - 1, '(",",I0))'
      else
         fmt = '(I0)'
      end if

      write(buffer,trim(fmt)) values
      buffer = adjustL(buffer)

   end function toString_integer1D




end module StringUtilities
