!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: AssertInteger
!
!> @brief
!! Handles a set of assertion methods useful for writing tests in integer
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 03 Feb 2008
!!
! REVISION HISTORY:
! 03 Feb 2008 - Initial Version
! 09 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module AssertInteger_mod
   use pFUnitException_mod
   implicit none
   private

   public :: arrayString
   public :: assertEqual

   interface assertEqual
      module procedure assertEqual_scalar
      module procedure assertEqual_vector
      module procedure assertEqual_scalar_vector
   end interface

   integer, parameter :: MAX_LEN_LINE    = 80
   integer, parameter :: MAX_LEN_MESSAGE = 80*10

contains

   !---------------------------------------------------------------------------
   !> Asserts that two scalar integers are equal.  If they are not, an 
   !! Exception is thrown with the given message.
   !!
   !! @param expected - expected integer value
   !! @param found -  found integer value
   !! @param message - the identifying message for the Exception
   !!
   !! @throw Exception - when two integers are not equal.
   !---------------------------------------------------------------------------
   subroutine assertEqual_scalar(expected, found, message)
      integer, intent(in) :: expected
      integer, intent(in) :: found
      character(len=*), optional, intent(in) :: message

      character(len=MAX_LEN_LINE) :: buffer
      character(len=MAX_LEN_MESSAGE) :: message_

      character(len=*), Parameter :: fmt_int_assertion='(a," Expected ",i0," but found ",i0,".")'
    
      if (expected == found) return

      message_ = 'Integer scalar assertion failed:'
      if (present(message)) message_ = trim(message_) // ' ' // trim(message)
      message_ = trim(message_) // '' // NEW_LINE('a')

      write(buffer,'(a,i0,a)') '       Expected:  ', expected, '' // NEW_LINE('a')
      message_ = trim(message_) // trim(buffer)
      write(buffer,'(a,i0,a)') '       but found: ', found
      message_ = trim(message_) // trim(buffer)
    
      call throw(Exception(message_))

   end subroutine assertEqual_scalar

   !---------------------------------------------------------------------------
   !> Asserts that two vector integers are equal.  If they are not, an
   !! Exception is thrown with the given message.
   !!
   !! @param expected - expected vector integers value
   !! @param found -  found vector integers value
   !! @param message - the identifying message for the Exception
   !!
   !! @throw Exception - when two vector integers are not equal.
   !---------------------------------------------------------------------------
   subroutine assertEqual_vector(expected, found, message)
      integer, intent(in) :: expected(:)
      integer, intent(in) :: found(:)
      character(len=*), optional, intent(in) :: message

      character(len=MAX_LEN_LINE) :: buffer
      character(len=MAX_LEN_MESSAGE) :: message_

    
      integer :: numExpected, numFound, minEntries
      logical :: sameSize, sameEntries
      integer :: i, firstDifference

      numExpected = size(expected)
      numFound    = size(found)
      minEntries = min(numExpected, numFound)

      sameSize = ( numExpected == numFound )
      sameEntries = all(expected(1:minEntries) == found(1:minEntries))
      if (sameSize .and. sameEntries) return

      message_ = 'Integer vector assertion failed:'
      if (present(message)) message_ = trim(message_) // ' ' // trim(message)
      message_ = trim(message_) // '' // NEW_LINE('a')

      if (.not. sameSize) message_ = trim(message_) // '       Arrays of different length.' // NEW_LINE('a')
      firstDifference = minEntries + 1
      do i = 1, minEntries
         if (expected(i) /= found(i)) then
             firstDifference = i
             exit
         end if
      end do
    
      message_ = trim(message_) // '       First difference at element ' // trim(toString(firstDifference)) // '.' // NEW_LINE('a')
      message_ = trim(message_) // '       Expected:  ' // trim(arrayString(expected)) // '' // NEW_LINE('a')
      message_ = trim(message_) // '       but found: ' // trim(arrayString(found))
    
      call throw(Exception(message_))

   end subroutine assertEqual_vector

   !---------------------------------------------------------------------------
   !> Asserts that two scalar and vector integers are equal.  If they are not,
   !! an Exception is thrown with the given message.
   !!
   !! @param expected - expected scalar and vector integer value
   !! @param found -  found scalar and vector integer value
   !! @param message - the identifying message for generic assertEqual
   !!
   !---------------------------------------------------------------------------
   subroutine assertEqual_scalar_vector(expected, found, message)
       integer, intent(in) :: expected
       integer, intent(in) :: found(:)
       character(len=*), optional, intent(in) :: message

       call assertEqual(spread(expected, 1, size(found)), found, message)

   end subroutine assertEqual_scalar_vector

   !---------------------------------------------------------------------------
   !> Converts the given number in integer to string 
   !!
   !! @param n - given number in integer 
   !! 
   !! @return string that is converted from integer 
   !!
   !! @note untested trivial utility function
   !---------------------------------------------------------------------------
   function toString(n) result(string)
      integer, intent(in) :: n
      character(len=10) :: string
      write(string,'(i0)') n
   end function toString

   !---------------------------------------------------------------------------
   !> Converts numbers in array integer to string 
   !!
   !! @param array - numbers in integer array 
   !!
   !! @return numbers in string that is converted from integer array
   !---------------------------------------------------------------------------
   function arrayString(array) result(string)
      integer, intent(in) :: array(:)
      character(len=MAX_LEN_MESSAGE) :: string
      character(len=MAX_LEN_LINE) :: frmt
      integer :: n

      n = size(array)
      select case (n)
      case (1)
         write(string,'("(/ ", i0, " /)")') array
      case (2)
         write(string,'("(/ ", i0, ", ", i0, " /)")') array
      case (3:)
         write(frmt,'("(",a,i0,a,")")') '"(/ ",',n-1,'(i0,", "),i0," /)"'
         write(string, frmt) array
      end select

   end function arrayString

end module AssertInteger_mod
