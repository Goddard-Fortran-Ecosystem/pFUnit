module AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   use StringUtilities_mod
   implicit none
   private
   
   public :: fail

   public :: assertTrue
   public :: assertFalse
   public :: assertEqual
   public :: assertExceptionRaised
   public :: assertSameShape

   ! Utility procedures
   public :: conformable
   public :: nonConformable
   
   public :: UnusableArgument

   interface fail
      module procedure fail_
      module procedure fail_withMessage
      module procedure fail_withMessageFileAndLine
   end interface fail

   interface assertTrue
      module procedure assertTrue_
      module procedure assertTrue_withMessage
   end interface

   interface assertFalse
      module procedure assertFalse_
      module procedure assertFalse_withMessage
   end interface

   interface assertEqual
      module procedure assertEqualString
   end interface

   interface assertExceptionRaised
      module procedure assertExceptionRaisedBasic
      module procedure assertExceptionRaisedMessage
   end interface assertExceptionRaised

   ! Arguments of the type below are used to force keyword arguments
   ! for optional arguments. 
   type UnusableArgument
   end type UnusableArgument


contains

   subroutine fail_(location)
      type (SourceLocation), optional, intent(in) :: location
      call fail(NULL_MESSAGE, location)
   end subroutine fail_
   
   subroutine fail_withMessageFileAndLine(message, file, line)
      character(len=*), intent(in) :: message
      character(len=*), intent(in) :: file
      integer, intent(in) :: line
      call fail(message, SourceLocation(file, line))
   end subroutine fail_withMessageFileAndLine

   subroutine fail_withMessage(message, location)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      call throw(message, location)
   end subroutine fail_withMessage

   subroutine assertTrue_(condition, unused, file, line)
      logical, intent(in) :: condition
      type (UnusableArgument), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line
      call assertTrue(condition, NULL_MESSAGE, file, line)
   end subroutine assertTrue_

   subroutine assertTrue_withMessage(condition, message, file, line)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line
      if (.not. condition) call throw(trim(message), SourceLocation(file, line))
   end subroutine assertTrue_withMessage

   subroutine assertExceptionRaisedBasic()
      use Exception_mod, only: throw, catch

      if (.not. catch()) then
         call throw('Failed to throw exception.')
      end if

   end subroutine assertExceptionRaisedBasic

   subroutine assertExceptionRaisedMessage(message)
      use Exception_mod, only: throw, catch
      character(len=*), intent(in) :: message

      if (.not. catch(message)) then
         call throw('Failed to throw exception: <' // trim(message) // '>')
      end if

   end subroutine assertExceptionRaisedMessage

   subroutine assertSameShape(shapeA, shapeB, message, file, line)
      integer, intent(in) :: shapeA(:)
      integer, intent(in) :: shapeB(:)
      character(len=*), optional, intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line

      character(len=MAXLEN_MESSAGE) :: throwMessage
      character(len=MAXLEN_MESSAGE) :: message_

      message_ = NULL_MESSAGE
      if (present(message)) message_ = message

      if (nonConformable(shapeA, shapeB)) then
         throwMessage = 'nonconforming arrays - expected shape: ' // &
              & trim(toString(shapeA)) // ' but found shape: ' // &
              & trim(toString(shapeB))
         call throw(appendWithSpace(message_, throwMessage), &
              & SourceLocation(file, line))
      end if
         
   end subroutine assertSameShape

   logical function conformable(shapeA, shapeB)
      integer, intent(in) :: shapeA(:)
      integer, intent(in) :: shapeB(:)

      if (size(shapeA) == 0 .or. size(shapeB) == 0) then
         conformable = .true.
         return
      end if

      conformable = size(shapeA) == size(shapeB)
      if (conformable) then
         conformable = all(shapeA == shapeB)
      end if
   end function conformable

   logical function nonConformable(shapeA, shapeB)
      integer, intent(in) :: shapeA(:)
      integer, intent(in) :: shapeB(:)

      nonConformable = .not. conformable(shapeA, shapeB)

   end function nonConformable


   subroutine assertFalse_(condition, unused, file, line)
      logical, intent(in) :: condition
      type (UnusableArgument), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line

      call assertFalse(condition, NULL_MESSAGE, file, line)
   end subroutine assertFalse_

   subroutine assertFalse_withMessage(condition, message, file, line)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line

      call assertTrue(.not. condition, message, file, line)
   end subroutine assertFalse_withMessage

   subroutine assertEqualString(expected, found)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: i
      integer :: numSameCharacters

      if (trim(expected) /= trim(found)) then
         numSameCharacters = 0
         do i = 1, min(len_trim(expected), len_trim(found))
            if (expected(i:i) /= found(i:i)) exit
            numSameCharacters = numSameCharacters + 1
         end do
         write(throwMessage,'((a,a),2(a,a,a,a),(a,a,a))') 'String assertion failed:', new_line('A'), &
              & '    expected: <"', trim(expected), '">', new_line('A'), &
              & '   but found: <"', trim(found), '">', new_line('A'), &
              & '  first diff:   ', repeat('-', numSameCharacters), '^'
         call throw(throwMessage)
      end if
   end subroutine assertEqualString

end module AssertBasic_mod
