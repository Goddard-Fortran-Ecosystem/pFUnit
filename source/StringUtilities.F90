! This module converts integers/real's to strings of a specific format
! for unit test diagnostics.  Basically just a wrapper for Fortran
! formatting, but functional programming provides a better style in many
! situations.
! 
! Further control of field width could be added at a later time.
!

module StringUtilities_mod

  use Params_mod, only : r32, r64

   implicit none
   private
   
   public :: toString
   public :: appendWithSpace
   public :: MAXLEN_STRING
   public :: nullTerminate


   integer, parameter :: MAXLEN_STRING = 80
   interface toString
      module Procedure toString_real64Scalar
      module Procedure toString_realScalar
      module Procedure toString_complexScalar
      module Procedure toString_integerScalar
      module Procedure toString_integer1D
   end interface

contains

!   character(len=MAXLEN_STRING) function toString_complex64Scalar(value) result(buffer)
!      complex(kind=r64), intent(in) :: value
!
!      write(buffer,'(2(SP,G14.7))') value
!      buffer = adjustL(buffer)
!
!   end function toString_complexScalar

   character(len=MAXLEN_STRING) function toString_complexScalar(value) result(buffer)
      complex, intent(in) :: value

      write(buffer,'(2(SP,G14.7))') value
      buffer = adjustL(buffer)

   end function toString_complexScalar

   character(len=MAXLEN_STRING) function toString_real64Scalar(value) result(buffer)
      real(kind=r64), intent(in) :: value

      write(buffer,'(SP,G14.7)') value
      buffer = adjustL(buffer)

    end function toString_real64Scalar

   character(len=MAXLEN_STRING) function toString_realScalar(value) result(buffer)
      real(kind=r32), intent(in) :: value

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

   function nullTerminate(string) result(nullTerminatedString)
      use iso_c_binding
      character(len=*), intent(in) :: string
      character(len=:), allocatable :: nullTerminatedString

      nullTerminatedString = trim(string) // C_NULL_CHAR

   end function nullTerminate

end module StringUtilities_mod
