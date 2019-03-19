!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: StringUtilities
!
!> @brief
!! A collection of utilities used throughout the framework.
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 05 Sep 2014 - Added options for working with whitespace including
!               ignore, trim, or keep.  Note: trimAll trims both
!               sides, while trimTrailingWhitespace is more like
!               Fortran's trim. MLR
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

module PF_StringUtilities

  use PF_Params, only : r32, r64
  use PF_Params, only : i32, i64

   implicit none
   private
   
   public :: toString
   public :: appendWithSpace
   public :: MAXLEN_BUFFER
   public :: nullTerminate
   public :: unlessScalar
   public :: WhitespaceOptions, IGNORE_ALL, TRIM_ALL, KEEP_ALL, IGNORE_DIFFERENCES
   public :: whitespacep, trimAll, trimTrailingWhitespace
   public :: to_lower

   integer, parameter :: MAXLEN_BUFFER = 80

   interface toString
      module Procedure toString_real64Scalar
      module Procedure toString_realScalar
      module Procedure toString_complex64Scalar
      module Procedure toString_complexScalar
      module Procedure toString_integerScalar_i32
      module Procedure toString_integer1D_i32
      module Procedure toString_integerScalar_i64
      module Procedure toString_integer1D_i64
   end interface

   character(len=*), parameter :: r32fmtStr = 'SP,G14.7'
   character(len=*), parameter :: r64fmtStr = 'SP,G14.7'
   character(len=*), parameter :: r32fmt1 = '('//r32fmtStr//')'
   character(len=*), parameter :: r64fmt1 = '('//r64fmtStr//')'

   character(len=*), parameter :: c32fmt1 = '("z=(",'//r32fmt1//',",",'//r32fmt1//',")")'
   character(len=*), parameter :: c64fmt1 = '("z=(",'//r64fmt1//',",",'//r64fmt1//',")")'

!   enum, bind(c) :: WhitespaceOptions
   type WhitespaceOptions
      integer value
   end type WhitespaceOptions
   enum, bind(c)
      enumerator :: IGNORE_ALL_, TRIM_ALL_, KEEP_ALL_, IGNORE_DIFFERENCES_
   end enum
   type (WhitespaceOptions), parameter :: &
        & IGNORE_ALL=WhitespaceOptions(IGNORE_ALL_), &
        & TRIM_ALL  =WhitespaceOptions(TRIM_ALL_), &
        & KEEP_ALL  =WhitespaceOptions(KEEP_ALL_), &
        & IGNORE_DIFFERENCES =WhitespaceOptions(IGNORE_DIFFERENCES_)

contains

   function toString_complex64Scalar(value) result(string)
      character(:), allocatable :: string
      complex(kind=r64), intent(in) :: value
      character(len=MAXLEN_BUFFER) :: buffer

      write(buffer,c64fmt1) value
      string = adjustL(trim(buffer))

    end function toString_complex64Scalar

   function toString_complexScalar(value) result(string)
      character(:), allocatable :: string
      complex, intent(in) :: value
      character(len=MAXLEN_BUFFER) :: buffer

      write(buffer,c32fmt1) value
      string = adjustL(trim(buffer))

   end function toString_complexScalar

   function toString_real64Scalar(value) result(string)
      character(:), allocatable :: string
      real(kind=r64), intent(in) :: value
      character(len=MAXLEN_BUFFER) :: buffer

      write(buffer,'(SP,G14.7)') value
      string = adjustL(trim(buffer))

    end function toString_real64Scalar

   function toString_realScalar(value) result(string)
      character(:), allocatable :: string
      real(kind=r32), intent(in) :: value
      character(len=MAXLEN_BUFFER) :: buffer

      write(buffer,'(SP,G14.7)') value
      string = adjustL(trim(buffer))

   end function toString_realScalar

   function toString_integerScalar_i32(value) result(string)
      character(:), allocatable :: string
      integer(kind=i32), intent(in) :: value
      character(len=20) :: fmt
      character(len=MAXLEN_BUFFER) :: buffer

      fmt = '(I0)'
      write(buffer,trim(fmt)) value
      string = adjustL(trim(buffer))

    end function toString_integerScalar_i32

   function toString_integer1D_i32(arrayShape) result(string)
      character(:), allocatable :: string
      integer(kind=i32), intent(in) :: arrayShape(:)
      character(len=MAXLEN_BUFFER) :: buffer

      select case (size(arrayShape)) ! rank
      case (0) ! scalar
         string = '0'
      case (1)
         write(buffer,'(i0)') arrayShape(1)
         string = trim(buffer)
      case (2:)
         write(buffer,'(i0,14(",",i0:))') arrayShape(1:)
         string = trim(buffer)
      end select

      string = '[' // string // ']'
    end function toString_integer1D_i32

   function toString_integerScalar_i64(value) result(string)
      character(:), allocatable :: string
      integer(kind=i64), intent(in) :: value
      character(len=20) :: fmt
      character(len=MAXLEN_BUFFER) :: buffer

      fmt = '(I0)'
      write(buffer,trim(fmt)) value
      string = adjustl(trim(buffer))

    end function toString_integerScalar_i64

   function toString_integer1D_i64(arrayShape) result(string)
      character(:), allocatable :: string
      integer(kind=i64), intent(in) :: arrayShape(:)
      character(len=MAXLEN_BUFFER) :: buffer

      select case (size(arrayShape)) ! rank
      case (0) ! scalar
         string = '0'
      case (1)
         write(buffer,'(i0)') arrayShape(1)
         string = trim(buffer)
      case (2:)
         write(buffer,'(i0,14(",",i0:))') arrayShape(1:)
         string = trim(buffer)
      end select

      string = '[' // string // ']'
    end function toString_integer1D_i64

   
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
      use, intrinsic :: iso_c_binding
      character(len=*), intent(in) :: string
      character(:), allocatable :: nullTerminatedString

      nullTerminatedString = trim(string) // C_NULL_CHAR

   end function nullTerminate

   function unlessScalar(vShape,string) result(retString)
     integer, intent(in), dimension(:) :: vShape
     character(len=*), intent(in) :: string
     character(:), allocatable :: retString
     retString=""
     if(size(vShape).ne.0)then
        retString=string
     end if
   end function unlessScalar

   logical function whitespacep(c)
     character, intent(in) :: c
     integer, parameter :: iachar_spc = 32, iachar_tab = 9
     whitespacep = &
          & iachar(c) .eq. iachar_spc .or. &
          & iachar(c) .eq. iachar_tab
   end function whitespacep

   function trimAll(s) result(trimmed)
     character(len=*), intent(in) :: s
     character(:), allocatable :: trimmed
     integer :: i,lenS,leadingWhite,trailingWhite,lenTrimmed

     lenS = len(s)

     leadingWhite = 0
     do i = 1,lenS
        if (whitespacep(s(i:i))) then
           leadingWhite = leadingWhite + 1
        else
           exit
        end if
     end do

     trailingWhite = 0
     do i = lenS,leadingWhite+1,-1
        if (whitespacep(s(i:i))) then
           trailingWhite = trailingWhite + 1
        else
           exit
        end if
     end do
     lenTrimmed = lenS-leadingWhite-trailingWhite
     
     allocate(character(lenTrimmed) :: trimmed)
     do i = 1,lenTrimmed
        trimmed(i:i) = s(i+leadingWhite:i+leadingWhite)
     end do
   end function trimAll

   function trimTrailingWhitespace(s) result(trimmed)
     character(len=*), intent(in) :: s
     character(:), allocatable :: trimmed
     integer :: i,lenS
     integer :: trailingWhite,lenTrimmed
     integer :: leadingWhite

     lenS = len(s)

     leadingWhite = 0
     do i = 1,lenS
        if (whitespacep(s(i:i))) then
           leadingWhite = leadingWhite + 1
        else
           exit
        end if
     end do

     trailingWhite = 0
     do i = lenS,leadingWhite+1,-1
        if (whitespacep(s(i:i))) then
           trailingWhite = trailingWhite + 1
        else
           exit
        end if
     end do

     lenTrimmed = lenS-trailingWhite
     trimmed = s(1:lenTrimmed)

   end function trimTrailingWhitespace

  function to_lower(string) result(converted_string)
    character(:), allocatable :: converted_string
    character(*), intent(in) :: string

    integer :: i, n, ascii
    integer :: delta

    delta = iachar('a') - iachar('A')
    n = len(string)
    allocate(character(len=n) :: converted_string)
    do i = 1, n
       ascii = iachar(string(i:i))
       if (ascii >= iachar('A') .and. ascii <= iachar('Z')) then
          converted_string(i:i) = achar(ascii + delta)
       else
          converted_string(i:i) = string(i:i)
       end if
    end do
  end function to_lower

end module PF_StringUtilities
