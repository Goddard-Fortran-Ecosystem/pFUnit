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
   end interface fail

   interface assertTrue
      module procedure assertTrue_
   end interface

   interface assertFalse
      module procedure assertFalse_
   end interface

   interface assertEqual
      module procedure assertEqualString_
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

   subroutine fail_(message, location)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      call throw(message, location)
    end subroutine fail_

   subroutine assertTrue_(condition, message, location)
      logical, intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      character(len=:), allocatable :: message_
      type (SourceLocation) :: location_

      if(present(message))then
         message_ = message
      else
         message_ = NULL_MESSAGE
      end if

      if(present(location))then
         location_ = location
      else
         location_ = UNKNOWN_SOURCE_LOCATION
      end if

      if (.not. condition) call throw(trim(message), location)
    end subroutine assertTrue_

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

   subroutine assertSameShape(shapeA, shapeB, message, location)
      integer, intent(in) :: shapeA(:)
      integer, intent(in) :: shapeB(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage
      character(len=MAXLEN_MESSAGE) :: message_

      message_ = NULL_MESSAGE
      if (present(message)) message_ = message


      if (nonConformable(shapeA, shapeB)) then
         throwMessage = 'nonconforming arrays - expected shape: ' // &
              & trim(toString(shapeA)) // ' but found shape: ' // &
              & trim(toString(shapeB))

         call throw(appendWithSpace(message_, throwMessage), &
              & location)
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

   subroutine assertFalse_(condition, message, location)
      logical, intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(.not. condition, message, location)
   end subroutine assertFalse_

   subroutine assertEqualString_(expected, found, message, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: message_
      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: i
      integer :: numSameCharacters

      if(present(message))then
         message_ = message
      else
         message_ = NULL_MESSAGE
      end if

      if(present(location))then
         location_ = location
      else
         location_ = UNKNOWN_SOURCE_LOCATION
      end if

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
         call throw(appendWithSpace(message, throwMessage), location_)
      end if

   end subroutine assertEqualString_

end module AssertBasic_mod
