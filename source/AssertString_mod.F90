!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: AssertString
!
!> @brief
!! Handles a set of assertion methods useful for writing tests in string
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
module AssertString_mod
  use pFUnitException_mod
  implicit none
  private

  public :: assertEqual
  public :: trimWhiteSpace
  public :: assertFailedAssert

  interface assertEqual
     module procedure assertEqualString
  end interface

  integer, parameter :: MAX_LEN_LINE    = 80
  integer, parameter :: MAX_LEN_MESSAGE = 80*10

contains

   !---------------------------------------------------------------------------
   !> Asserts that two strings are equal.  If they are not, an
   !! Exception is thrown with the given message.
   !!
   !! @param expected - expected value in string  
   !! @param found -  found value in string
   !! @param message - the identifying message for the Exception
   !!
   !! @throw Exception - when two strings are not equal.
   !---------------------------------------------------------------------------
   subroutine assertEqualString(expected, found, message, ignoreWhiteSpace)
      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found
      character(len=*), intent(in), optional :: message
      logical, optional, intent(in) :: ignoreWhiteSpace

      logical :: ignoreWhiteSpace_
      integer :: lengthExpected
      integer :: lengthFound
      integer :: minLength
      integer :: i, startDifference
      character(len=MAX_LEN_LINE) :: buffer
      character(len=MAX_LEN_MESSAGE) :: message_
      logical :: sameLength, sameContents, same

      character(len=len(expected)) :: expected_
      character(len=len(found))    :: found_

      ignoreWhiteSpace_ = .false.
      if (present(ignoreWhiteSpace)) ignoreWhiteSpace_ = ignoreWhiteSpace

      if (ignoreWhiteSpace_) then
         expected_ = trimWhiteSpace(expected)
         found_    = trimWhiteSpace(found)
      else
         expected_ = expected
         found_    = found
      end if

      lengthExpected = len_trim(expected_)
      lengthFound    = len_trim(found_)
      minLength = min(lengthExpected, lengthFound)

      sameLength   = ( lengthExpected == lengthFound )
      sameContents = ( expected_(:minLength) == found_(:minLength) )
      same = ( sameLength .and. sameContents )

      if (same) return

      message_ = 'String assertion failed: '
      if (present(message)) message_ = trim(message_) // ' ' // trim(message)
      message_ = trim(message_) // '' // NEW_LINE('a')

      startDifference = 0
      if (.not. sameLength) then
         message_ = trim(message_) // '       Strings of unequal length.' // NEW_LINE('a')
         startDifference = minLength + 1
      end if

      do i = 1, minLength
         if (expected_(i:i) /= found_(i:i)) then
            startDifference = i
         end if
      end do

      write(buffer,'(a,i0,a)')  '       Differences begin at position ',startDifference,'.' // NEW_LINE('a')
      message_ = trim(message_) // trim(buffer)
      message_ = trim(message_) // '       Expected:  "' // trim(expected) // '"' // NEW_LINE('a')
      message_ = trim(message_) // '       but found: "' // trim(found) // '"'

      call throw(Exception(message_))

   end subroutine assertEqualString

   !---------------------------------------------------------------------------
   !> Trims out whitespace characters and return new string
   !!
   !! @param string - given string for trim-out
   !! 
   !! @return string after trimming out whitespace characters
   !---------------------------------------------------------------------------
   function trimWhiteSpace(string) result(newString)
      character(len=*), intent(in) :: string
      character(len=len(string))   :: newString

      integer :: i, j, n
      logical :: previousCharacterIsWhiteSpace
    
      n = len_trim(string)
      j = 0
      newString=''
      previousCharacterIsWhiteSpace = .true. ! always remove leading spaces
      do i = 1, n
         if (isWhiteSpace(string(i:i))) then
            if (previousCharacterIsWhiteSpace) cycle
            previousCharacterIsWhiteSpace = .true.
         else
            previousCharacterIsWhiteSpace = .false.
         end if
         j = j + 1
         newString(j:j) = string(i:i)
      end do

   contains

      !---------------------------------------------------------------------------
      !> Determines if the given character is whitespace.
      !!
      !! @param char - given charactor to be checked.
      !! 
      !! @return .true. if char is whitespace.  Otherwise, it is .false.
      !---------------------------------------------------------------------------
      logical function isWhiteSpace(char)
         character :: char

         select case (char)
         case (' ')
            isWhiteSpace = .true.
         case default
            isWhiteSpace = .false.
         end select

      end function isWhiteSpace

   end function trimWhiteSpace
  
   !---------------------------------------------------------------------------
   !> Asserts that string is expected to be failed.  If it is not, an
   !! Exception is thrown with the given message.
   !!
   !! @param expected - expected string to be failed
   !! @param message - the identifying message for the Exception
   !!
   !! @throw Exception - string is not failed as expected.
   !---------------------------------------------------------------------------
   subroutine assertFailedAssert(expected, message)
      character(len=*), optional, intent(in) :: expected
      character(len=*), optional, intent(in) :: message
    
      logical :: detected
      character(len=MAX_LEN_MESSAGE) :: message_
      type (Exception_type) :: anException

      if (present(expected)) then
         detected = catch(expected)
      else
         detected = catch()
      end if

      if (.not. detected) then
         if (present(expected)) then
            message_ = 'Failure: specified assertion failure did not occur.'
         else
            message_ = 'Failure: expected assertion failure did not occur.'
         end if
         if (present(message)) message_ = trim(message)
       
         call throw(Exception(message_))
      end if

   end subroutine assertFailedAssert

end module AssertString_mod
