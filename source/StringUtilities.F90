! This module converts integers/real's to strings of a specific format
! for unit test diagnostics.  Basically just a wrapper for Fortran
! formatting, but functional programming provides a better style in many
! situations.
! 
! Further control of field width could be added at a later time.
!

module StringUtilities_mod
   implicit none
   private
   
   public :: toString
   public :: appendWithSpace
   public :: MAXLEN_STRING


   integer, parameter :: MAXLEN_STRING = 80
   interface toString
      module Procedure toString_realScalar
      module Procedure toString_integerScalar
      module Procedure toString_integer1D
   end interface

contains

   character(len=MAXLEN_STRING) function toString_realScalar(value) result(buffer)
      real, intent(in) :: value

      write(buffer,'(SP,G14.7)') value
      buffer = adjustL(buffer)

   end function toString_realScalar

   character(len=MAXLEN_STRING) function toString_integerScalar(value) result(buffer)
      integer, intent(in) :: value
      character(len=20) :: fmt

      fmt = '(I0)'
      write(buffer,trim(fmt)) value
      buffer = adjustL(buffer)

   end function toString_integerScalar

   function toString_integer1D(arrayShape) result(string)
      integer, intent(in) :: arrayShape(:)
      character(len=MAXLEN_STRING) :: string

      integer :: i
      
      select case (size(arrayShape)) ! rank
      case (0) ! scalar
         string = '0'
      case (1)
         write(string,'(i0)') arrayShape(1)
      case (2:)
         write(string,'(i0,14(",",i0:))') arrayShape(1:)
      end select

      string = '[' // trim(string) // ']'
   end function toString_integer1D

   ! Joins two strings with a space separator unless first string is
   ! empty.
   function appendWithSpace(a, b) result(ab)
      character(len=*), intent(in) :: a
      character(len=*), intent(in) :: b
      character(len=len_trim(a)+1+len_trim(b)) :: ab

      if (len_trim(a) > 0) then
         ab = trim(a) // ' ' // trim(b)
      else
         ab = trim(b)
      end if

   end function appendWithSpace

end module StringUtilities_mod
