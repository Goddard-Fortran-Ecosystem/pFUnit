!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: AssertBasic
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
module AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   use StringConversionUtilities_mod
   implicit none
   private
   
   public :: fail
   public :: assertFail

   public :: assertTrue
   public :: assertFalse
   public :: assertEqual
   public :: assertExceptionRaised
   public :: assertSameShape

   public :: assertAny
   public :: assertAll
   public :: assertNone
   public :: assertNotAll

   public :: assertIsNaN
   public :: assertIsFinite


   ! Utility procedures
   public :: conformable
   public :: nonConformable
   
   public :: UnusableArgument

   ! from StringConversionUtilities
   public :: WhitespaceOptions
   public :: pleaseIgnore, pleaseTrim, pleaseKeep

   interface fail
      module procedure fail_
   end interface fail

   interface assertFail
      module procedure fail_
   end interface assertFail

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

   interface assertIsNaN
      module procedure assertIsNan_single
      module procedure assertIsNan_double
   end interface assertIsNaN

   interface assertIsFinite
      module procedure assertIsFinite_single
      module procedure assertIsFinite_double
   end interface assertIsFinite

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

      if(present(message))then
         message_ = message
      else
         message_ = NULL_MESSAGE
      end if

      if (.not. condition) call throw(trim(message), location)
    end subroutine assertTrue_

   subroutine assertExceptionRaisedBasic(location)
      use Exception_mod, only: throw, catch
      type (SourceLocation), optional, intent(in) :: location

      if (.not. catch()) then
         call throw('Failed to throw exception.', location)
      end if

   end subroutine assertExceptionRaisedBasic

   subroutine assertExceptionRaisedMessage(message, location)
      use Exception_mod, only: throw, catch
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      if (.not. catch(message)) then
         call throw('Failed to throw exception: <' // trim(message) // '>', &
              & location)
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

   subroutine assertEqualString_(expected, found, message, location, &
        & forWhitespace)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      type (WhitespaceOptions), optional, intent(in) :: &
           & forWhitespace

      character(len=:), allocatable :: message_
      type (SourceLocation) :: location_
      type (WhitespaceOptions) :: forWhitespace_

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: i, j
      integer :: numI, numJ
      integer :: numSameCharacters

      integer, parameter :: iachar_spc = 32, iachar_tab = 9

      logical :: checkForDifference, charDifference
      logical :: throwException
      character(len=:), allocatable :: expected_, found_

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

      if(present(forWhitespace))then
         forWhitespace_ = forWhitespace
      else
         forWhitespace_ = pleaseTrim
      end if

      select case (forWhitespace_%value)
      case (pleaseTrim%value)
         expected_ = trimAll(expected)
         found_    = trimAll(found)
      case (pleaseIgnore%value)
         expected_ = trimAll(expected)
         found_    = trimAll(found)
      case (pleaseKeep%value)
         expected_ = expected
         found_    = found
      end select

      ! Determine if we need to iterate through the characters in the strings.
      ! Trim: ignore leading & trailing white space.  
      ! Ignore: ignore all white space.
      ! Keep: white space is significant.
      ! Worry:  Original code written to print out trimmed strings.  Not sure what effect
      ! Keep will have.
      checkForDifference = .true.
      select case (forWhitespace_%value)
         case (pleaseTrim%value)
            checkForDifference = expected_ /= found_
            numI = len(expected_); numJ = len(found_)
         case (pleaseIgnore%value)
            checkForDifference = expected_ /= found_
            numI = len(expected_); numJ = len(found_)
!            numI = len_trim(expected); numJ = len_trim(found)
         case (pleaseKeep%value)
            checkForDifference = expected_ /= found_
            numI = len(expected_); numJ = len(found_)
!            numI = len(expected); numJ = len(found)
         case default
            write(throwMessage,'(a)')&
                 & 'assertEqualString_InternalError: ' &
                 & // 'Unknown case for handling Whitespace'
            call throw(appendWithSpace(message,throwMessage), location_)
      end select


      throwException = .false.
!      if (trim(expected) /= trim(found)) then
      if (checkForDifference) then
         numSameCharacters = 0

         ! Cycle over both strings, compare each element, skipping if needed.
         i = 1; j = 1
         do
            ! Is a string traversal complete?
            if ( i .gt. numI .or. j .gt. numJ ) then
               ! If both made it to end, exit ok, else continue other traverse.
               if ( i .gt. numI .and. j .gt. numJ ) exit
            end if

            ! Ignore?  Then skip that element.  Skip on i first, then j.
            select case (forWhitespace_%value)
            case (pleaseIgnore%value)
               if( i .le. numI ) then
                  if(whitespacep(expected_(i:i)))then
                     i=i+1; cycle
                  end if
               end if
               if( j .le. numJ ) then
                  if(whitespacep(found_(j:j)))then
                     j=j+1; cycle
                  end if
               end if
            end select

            ! A character is not white space -- fail if a traverse is complete.
            if ( i .gt. numI .or. j .gt. numJ ) then
               throwException = .true. ; exit
            end if

            ! Both characters are not whitespace:  fail if unequal.
            if (expected_(i:i) /= found_(j:j)) then
               throwException = .true. ; exit
            end if

            ! Consume both of the equal characters.
            i=i+1; j=j+1; numSameCharacters = numSameCharacters + 1

         end do

         if (throwException) then
            select case (forWhitespace_%value)
            case (pleaseTrim%value)
               expected_ = trimTrailingWhitespace(expected)
               found_    = trimTrailingWhitespace(found)
            case (pleaseIgnore%value)
               expected_ = trimTrailingWhitespace(expected)
               found_    = trimTrailingWhitespace(found)
            case (pleaseKeep%value)
               expected_ = expected
               found_    = found
            end select

            write(throwMessage,'((a,a),2(a,a,a,a),(a,a,a))') &
                 & 'String assertion failed:', new_line('A'), &
                 & '    expected: <"', expected_, '">', new_line('A'), &
                 & '   but found: <"', found_, '">', new_line('A'), &
                 & '  first diff:   ', repeat('-', numSameCharacters), '^'
            call throw(appendWithSpace(message, throwMessage), location_)

         end if

      end if

      !contains

   end subroutine assertEqualString_

   subroutine assertAny(conditions, message, location)
      logical, intent(in) :: conditions(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(any(conditions), message, location)
   end subroutine assertAny

   subroutine assertAll(conditions, message, location)
      logical, intent(in) :: conditions(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(all(conditions), message, location)
   end subroutine assertAll

   subroutine assertNone(conditions, message, location)
      logical, intent(in) :: conditions(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(.not. any(conditions), message, location)
   end subroutine assertNone

   subroutine assertNotAll(conditions, message, location)
      logical, intent(in) :: conditions(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(.not. all(conditions), message, location)
   end subroutine assertNotAll


   subroutine assertIsNaN_single(x, message, location)
      use Params_mod, only: r32
#ifndef __GFORTRAN__
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
#endif
      real(kind=r32), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
#ifdef __GFORTRAN__
      call assertTrue(isNaN(x), message, location)
#else
      call assertTrue(ieee_is_nan(x), message, location)
#endif
   end subroutine assertIsNaN_single

   subroutine assertIsNaN_double(x, message, location)
      use Params_mod, only: r64
#ifndef __GFORTRAN__
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
#endif
      real(kind=r64), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
#ifdef __GFORTRAN__
      call assertTrue(isNaN(x), message, location)
#else
      call assertTrue(ieee_is_nan(x), message, location)
#endif
   end subroutine assertIsNaN_double

   subroutine assertIsFinite_single(x, message, location)
      use Params_mod, only: r32
#ifndef __GFORTRAN__
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
#endif
      real(kind=r32), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
#ifdef __GFORTRAN__
      call assertTrue(abs(x) <= huge(x), message, location)
#else
      call assertTrue(ieee_is_finite(x), message, location)
#endif
   end subroutine assertIsFinite_single

   subroutine assertIsFinite_double(x, message, location)
      use Params_mod, only: r64
#ifndef __GFORTRAN__
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
#endif
      real(kind=r64), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
#ifdef __GFORTRAN__
      call assertTrue(abs(x) <= huge(x), message, location)
#else
      call assertTrue(ieee_is_finite(x), message, location)
#endif
   end subroutine assertIsFinite_double


end module AssertBasic_mod
