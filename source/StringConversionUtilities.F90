!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: StringConversionUtilities
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
! This module converts integers/real's to strings of a specific format
! for unit test diagnostics.  Basically just a wrapper for Fortran
! formatting, but functional programming provides a better style in many
! situations.
! 
! Further control of field width could be added at a later time.
!

module StringConversionUtilities_mod

  use Params_mod, only : r32, r64

   implicit none
   private
   
   public :: toString
   public :: appendWithSpace
   public :: MAXLEN_STRING
   public :: nullTerminate
   public :: unlessScalar

   integer, parameter :: MAXLEN_STRING = 80
!   integer, parameter :: MAXLEN_STRING = 80*5

   interface toString
      module Procedure toString_real64Scalar
      module Procedure toString_realScalar
      module Procedure toString_complex64Scalar
      module Procedure toString_complexScalar
      module Procedure toString_integerScalar
      module Procedure toString_integer1D
   end interface

   character(len=*), parameter :: r32fmtStr = 'SP,G14.7'
   character(len=*), parameter :: r64fmtStr = 'SP,G14.7'
   character(len=*), parameter :: r32fmt1 = '('//r32fmtStr//')'
   character(len=*), parameter :: r64fmt1 = '('//r64fmtStr//')'

   character(len=*), parameter :: c32fmt1 = '("z=(",'//r32fmt1//',",",'//r32fmt1//',")")'
   character(len=*), parameter :: c64fmt1 = '("z=(",'//r64fmt1//',",",'//r64fmt1//',")")'

contains

   character(len=MAXLEN_STRING) function toString_complex64Scalar(value) result(buffer)
      complex(kind=r64), intent(in) :: value

!      write(buffer,'(2(SP,G14.7))') value
      write(buffer,c64fmt1) value
      buffer = adjustL(buffer)

    end function toString_complex64Scalar

   character(len=MAXLEN_STRING) function toString_complexScalar(value) result(buffer)
      complex, intent(in) :: value

!      write(buffer,'(2(SP,G14.7))') value
      write(buffer,c32fmt1) value
      buffer = adjustL(buffer)

   end function toString_complexScalar

   character(len=MAXLEN_STRING) function toString_real64Scalar(value) result(buffer)
      real(kind=r64), intent(in) :: value

      write(buffer,'(SP,G14.7)') value
!      write(buffer,r64fmt1) value
      buffer = adjustL(buffer)

    end function toString_real64Scalar

   character(len=MAXLEN_STRING) function toString_realScalar(value) result(buffer)
      real(kind=r32), intent(in) :: value

      write(buffer,'(SP,G14.7)') value
!      print *,'r32fmt1: ',r32fmt1
!      print *,'       : ','(SP,G14.7)'
!      print *,'=?     : ','(SP,G14.7)'.EQ.r32fmt1
!      write(buffer,r32fmt1)
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

   function unlessScalar(vShape,string) result(retString)
     integer, intent(in), dimension(:) :: vShape
     character(len=*), intent(in) :: string
     character(len=:), allocatable :: retString
     retString=""
     if(size(vShape).ne.0)then
        retString=string
     end if
   end function unlessScalar

end module StringConversionUtilities_mod
