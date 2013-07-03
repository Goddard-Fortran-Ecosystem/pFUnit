! This module converts integers/real's to strings of a specific format
! for unit test diagnostics.  Basically just a wrapper for Fortran
! formatting, but functional programming provides a better style in many
! situations.
! 
! Further control of field width could be added at a later time.
!

module ModuleProcedures
   implicit none
   private
   
   public :: iProc
   public :: MAXLEN_REAL_STRING


   integer, parameter :: MAXLEN_REAL_STRING = 25
   interface iProc
      module Procedure iProc_1
      module Procedure iProc_2
      module Procedure iProc_3
   end interface

contains

   character(len=MAXLEN_REAL_STRING) function iProc_1(value) result(buffer)
      real :: value
      
      write(buffer,'(SP,G14.7)') value
      buffer = adjustL(buffer)

   end function iProc_1

   character(len=MAXLEN_REAL_STRING) function iProc_2(value) result(buffer)
      integer :: value
      character(len=20) :: fmt
      fmt = '(I0)'

      write(buffer,trim(fmt)) value
      buffer = adjustL(buffer)

   end function iProc_2

   character(len=MAXLEN_REAL_STRING) function iProc_3(values) result(buffer)
      integer, intent(in) :: values(:)
      character(len=20) :: fmt

      if (size(values) > 1) then
         write(fmt,'("(I0,",I0,a)') size(values) - 1, '(",",I0))'
      else
         fmt = '(I0)'
      end if

      write(buffer,trim(fmt)) values
      buffer = adjustL(buffer)

   end function iProc_3




end module ModuleProcedures
