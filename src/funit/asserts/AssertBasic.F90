!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: AssertBasic
!
!> @brief
!! Provides fundamental assertions over the most basic types, a
!! foundation for providing test services to end users.
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note For assertions on strings whitespace may or may not be
!! significant to a test.  We now have several options for dealing
!! with whitespace via the optional argument
!! <code>Whitespace</code>.  These options are
!! IGNORE_ALL, TRIM_ALL, and KEEP_ALL.  Usage is as follows.
!!
!! <code>
!! call assertEqual(expectedString, foundString, &
!!                & Whitespace=IGNORE_ALL )
!! </code>
!! 
!! <strong>WhitespaceOptions:</strong>
!! <ul>
!! <li><strong>TRIM_ALL</strong> ignores leading and trailing whitespace.  </li>
!! <li><strong>KEEP_ALL</strong> keeps all whitespace as significant, even discriminating
!!            between tabs and spaces.</li>
!! <li><strong>IGNORE_ALL</strong> ignores all whitespace (spaces & tabs).</li>
!! </ul>
!!
!! Example usages can be seen in tests/Test_AssertBasic.F90 or
!! Examples/Simple/tests/helloWorld.pf.
!
! REVISION HISTORY:
!
! 05 Sep 2014 - Added polite whitespace options trim, ignore, and
!               keep. MLR
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module PF_AssertBasic
   use PF_Exception
   use PF_ExceptionList
   use PF_SourceLocation
   use PF_StringUtilities
#ifdef _REAL16_IEEE_SUPPORT
   use, intrinsic :: iso_fortran_env, only: REAL16
#endif
#ifdef _REAL32_IEEE_SUPPORT
   use, intrinsic :: iso_fortran_env, only: REAL32
#endif
#ifdef _REAL64_IEEE_SUPPORT
   use, intrinsic :: iso_fortran_env, only: REAL64
#endif
#ifdef _ISO_REAL80
   use, intrinsic :: iso_fortran_env, only: REAL80
#endif
#ifdef _REAL128_IEEE_SUPPORT
   use, intrinsic :: iso_fortran_env, only: REAL128
#endif
   implicit none
   private
   
   public :: fail
   public :: assertFail

   public :: assertTrue
   public :: assertFalse
   public :: assertSameShape
   public :: assertExceptionRaised

   public :: assertAny
   public :: assertAll
   public :: assertNone
   public :: assertNotAll

   public :: assertIsNaN
   public :: assertIsNotNaN
   public :: assertIsFinite
   public :: assertIsInfinite


   ! Utility procedures
   public :: conformable
   public :: nonConformable
   
   public :: UnusableArgument

   ! from StringUtilities
   public :: WhitespaceOptions
   public :: IGNORE_ALL, TRIM_ALL, KEEP_ALL, IGNORE_DIFFERENCES

   interface fail
      module procedure fail_
   end interface fail

   interface assertFail
      module procedure fail_
   end interface assertFail

   interface assertTrue
      module procedure assertTrue_
      module procedure assertTrue_1d_
   end interface

   interface assertFalse
      module procedure assertFalse_
      module procedure assertFalse_1d_
   end interface

   interface assertExceptionRaised
      module procedure assertExceptionRaisedBasic
      module procedure assertExceptionRaisedMessage
   end interface assertExceptionRaised

   interface assertIsNaN
#ifdef _REAL16_IEEE_SUPPORT
      module procedure assertIsNaN_real16
#endif
#ifdef _REAL32_IEEE_SUPPORT
      module procedure assertIsNaN_real32
#endif
#ifdef _REAL64_IEEE_SUPPORT
      module procedure assertIsNaN_real64
#endif
#ifdef _REAL80_IEEE_SUPPORT
      module procedure assertIsNaN_real80
#endif
#ifdef _REAL128_IEEE_SUPPORT
      module procedure assertIsNaN_real128
#endif
#ifdef _REAL256_IEEE_SUPPORT
      module procedure assertIsNaN_real256
#endif
   end interface assertIsNaN

   interface assertIsFinite
#ifdef _REAL16_IEEE_SUPPORT
      module procedure assertIsFinite_real16
#endif
#ifdef _REAL32_IEEE_SUPPORT
      module procedure assertIsFinite_real32
#endif
#ifdef _REAL64_IEEE_SUPPORT
      module procedure assertIsFinite_real64
#endif
#ifdef _REAL80_IEEE_SUPPORT
      module procedure assertIsFinite_real80
#endif
#ifdef _REAL128_IEEE_SUPPORT
      module procedure assertIsFinite_real128
#endif
#ifdef _REAL256_IEEE_SUPPORT
      module procedure assertIsFinite_real256
#endif
   end interface assertIsFinite

   interface assertIsNotNaN
#ifdef _REAL16_IEEE_SUPPORT
      module procedure assertIsNotNaN_real16
#endif
#ifdef _REAL32_IEEE_SUPPORT
      module procedure assertIsNotNaN_real32
#endif
#ifdef _REAL64_IEEE_SUPPORT
      module procedure assertIsNotNaN_real64
#endif
#ifdef _REAL80_IEEE_SUPPORT
      module procedure assertIsNotNaN_real80
#endif
#ifdef _REAL128_IEEE_SUPPORT
      module procedure assertIsNotNaN_real128
#endif
#ifdef _REAL256_IEEE_SUPPORT
      module procedure assertIsNotNaN_real256
#endif
   end interface assertIsNotNaN

   interface assertIsInfinite
#ifdef _REAL16_IEEE_SUPPORT
      module procedure assertIsInfinite_real16
#endif
#ifdef _REAL32_IEEE_SUPPORT
      module procedure assertIsInfinite_real32
#endif
#ifdef _REAL64_IEEE_SUPPORT
      module procedure assertIsInfinite_real64
#endif
#ifdef _REAL80_IEEE_SUPPORT
      module procedure assertIsInfinite_real80
#endif
#ifdef _REAL128_IEEE_SUPPORT
      module procedure assertIsInfinite_real128
#endif
#ifdef _REAL256_IEEE_SUPPORT
      module procedure assertIsInfinite_real256
#endif
   end interface assertIsInfinite

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

      message_ = NULL_MESSAGE
      if (present(message)) message_ = message

      if (.not. condition) call throw(trim(message_), location)
    end subroutine assertTrue_

   subroutine assertTrue_1d_(condition, message, location)
      logical, intent(in) :: condition(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: message_

      message_ = NULL_MESSAGE
      if (present(message)) message_ = message

      if (.not. all(condition)) call throw(trim(message_), location)
    end subroutine assertTrue_1d_


   subroutine assertExceptionRaisedBasic(location)
      use PF_ExceptionList, only: throw, catch
      type (SourceLocation), optional, intent(in) :: location

      if (.not. catch()) then
         call throw('Failed to throw exception.', location)
      end if

   end subroutine assertExceptionRaisedBasic


   subroutine assertExceptionRaisedMessage(message, location)
      use PF_ExceptionList, only: throw, catch
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

      character(len=:), allocatable :: throwMessage
      character(len=:), allocatable :: message_

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

   subroutine assertFalse_1d_(condition, message, location)
      logical, intent(in) :: condition(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(.not. condition, message, location)
   end subroutine assertFalse_1d_

   subroutine assertEqualString_(expected, found, message, location, &
        & whitespace)
      use PF_ExceptionList, only: throw

      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      type (WhitespaceOptions), optional, intent(in) :: &
           & whitespace

      character(len=:), allocatable :: message_
      type (WhitespaceOptions) :: whitespace_

      character(len=:), allocatable :: throwMessage
      integer :: i, j
      integer :: numI, numJ
      integer :: numSameCharacters

      integer, parameter :: iachar_spc = 32, iachar_tab = 9

      logical :: checkCharacterByCharacter
      logical :: throwException
      logical :: whitespaceYes
      character(len=:), allocatable :: expected_, found_

      throwException = .false.

      message_ = NULL_MESSAGE
      if (present(message)) message_ = message

      if(present(whitespace))then
         whitespace_ = whitespace
      else
         ! This is the default whitespace option.  TRIM_ALL is the legacy behavior.
         ! TODO:  Change default behavior to IGNORE_DIFFERENCES.
         whitespace_ = TRIM_ALL
      end if

      select case (whitespace_%value)
      case (TRIM_ALL%value)
         expected_ = trimAll(expected)
         found_    = trimAll(found)
      case (IGNORE_ALL%value)
         expected_ = trimAll(expected)
         found_    = trimAll(found)
      case (IGNORE_DIFFERENCES%value)
         expected_ = trimAll(expected)
         found_    = trimAll(found)
      case (KEEP_ALL%value)
         expected_ = expected
         found_    = found
      end select

      ! Determine if we need to iterate through the characters in the strings.
      ! Trim: ignore leading & trailing white space.  
      ! Ignore: ignore all white space.
      ! Keep: white space is significant.
      ! Worry:  Original code written to !print out trimmed strings.  Not sure what effect
      ! Keep will have.
      !print *,1000
      checkCharacterByCharacter = .true.
      select case (whitespace_%value)
      case (TRIM_ALL%value)
         ! Check to see if we have to do more work.
            checkCharacterByCharacter = expected_ /= found_
            numI = len(expected_); numJ = len(found_)

         case (IGNORE_ALL%value)
            checkCharacterByCharacter = expected_ /= found_
            numI = len(expected_); numJ = len(found_)

         case (IGNORE_DIFFERENCES%value)
            checkCharacterByCharacter = expected_ /= found_
            numI = len(expected_); numJ = len(found_)
            !print *,1001,whitespace_%value
            !print *,1002,'e="',expected_,'"'
            !print *,1003,'f="',found_,'"'

         case (KEEP_ALL%value)
            checkCharacterByCharacter = expected_ /= found_
            numI = len(expected_); numJ = len(found_)

         case default
            throwMessage = & 
                 & 'assertEqualString_InternalError: ' &
                 & // 'Unknown case for handling Whitespace'
            call throw(appendWithSpace(message_,throwMessage), location)
         end select

         ! Flag a check if zero-length arrays are involved.
         if ((numI .eq. 0) .or. (numJ .eq. 0)) then
            checkCharacterByCharacter = .true.
         end if

         ! Fortran implicitly pads strings of different lengths with spaces
         ! when comparing using /= or ==.  Detect them and compare carefully.
         if (numI .ne. numJ) then
            checkCharacterByCharacter = .true.
         end if
         
         !if (numI .eq. 0) then
         !   print *,'e: "'//expected_//'"'
         !   print *,'f: "'//found_//'"'
         !   print *,'?: ',checkCharacterByCharacter
         !   print *,'!: ',expected_ /= found_
         !   print *,'z: ',expected_ == found_
         !end if

      !print *,2000,whitespace_%value

!      if (trim(expected) /= trim(found)) then
      if (checkCharacterByCharacter) then
         numSameCharacters = 0

         ! Cycle over both strings, compare each element, skipping if needed.
         i = 1; j = 1
         countNumSameCharacters: do

            ! Is a string traversal complete?
            if ( i .gt. numI .or. j .gt. numJ ) then
               ! If both made it to end, exit ok, else continue other traverse.
               if ( i .gt. numI .and. j .gt. numJ ) exit
            end if

            ! Handle whitespace options.
            whitespaceYes = .false.
            if ( i .le. numI ) whitespaceYes = whitespacep(expected_(i:i)) 
            if ( j .le. numJ ) whitespaceYes = whitespaceYes .or. &
                & whitespacep(found_(j:j)) 

            if ( whitespaceYes ) then

               select case (whitespace_%value)

                  ! IGNORE_ALL?  Then skip that element.  Skip on i first, then j.
               case (IGNORE_ALL%value)
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

                  ! IGNORE_DIFFERENCES?
                  ! If either i & j start sequences that are white, skip past.
               case (IGNORE_DIFFERENCES%value)

                  !print *,2001

                  ! Because we expect to be dealing with trimmed strings
                  ! at this point, we need both sequences to be
                  ! whitespace, else fail.

                  if(  &
                       & .not.( &
                       &       whitespacep(expected_(i:i)) &
                       &       .and.whitespacep(found_(j:j))) ) then
                     throwException = .true.; exit
                  end if

                  !print *,2100

                  ! Skip past i's whitespace.
                  iWhitespace: if( i .le. numI ) then
                     iLoop: do
                        ! Found white char, skip.
                        if(whitespacep(expected_(i:i)))then
                           i=i+1; if (i .gt. numI) exit iLoop
                        else
                           exit iLoop
                        end if
                     end do iLoop
                     ! i now either indexes non-whitespace or is past its bound.
                  end if iWhitespace

                  ! Skip past j's whitespace.
                  jWhitespace: if( j .le. numJ ) then
                     jLoop: do
                        if(whitespacep(found_(j:j)))then
                           ! Found white char, skip.
                           j=j+1; if (j .gt. numJ) exit jLoop
                        else
                           exit jLoop
                        end if
                     end do jLoop
                     ! j now either indexes non-whitespace or is past its bound.
                  end if jWhitespace

                  ! If both finish at the same time, i,j .gt. numI, numJ.
                  ! should be an error condition.  Remember, we're
                  ! dealing with trimmed sequences.
                  !
                  !if ( i .gt. numI .and. j .gt. numJ ) then
                  !   ...cycle loop...
                  !end if

               end select

            end if

            ! Fail if a traverse is complete.
            !print *,2500,i,numI
            !print *,2501,j,numJ
            if ( i .gt. numI .or. j .gt. numJ ) then
               !print *,2502
               throwException = .true. ; exit
            end if

            ! A character is not white space!

            ! Both characters are not whitespace:  fail if unequal.
            !print *,3001,i,j,whitespace_%value,expected_,found_
            !print *,3002,expected_(i:i),found_(j:j)
            !print *,3003,expected_(i:i) /= found_(j:j)
            if (expected_(i:i) /= found_(j:j)) then
               !print *,3004,'x'
               throwException = .true. ; exit
            end if
            !print *,3005

            ! Consume both of the equal characters.
            i=i+1; j=j+1; numSameCharacters = numSameCharacters + 1

         end do countNumSameCharacters

         !print *,4000
         if (throwException) then
            select case (whitespace_%value)
            case (TRIM_ALL%value)
               expected_ = trimTrailingWhitespace(expected)
               found_    = trimTrailingWhitespace(found)
            case (IGNORE_ALL%value)
               expected_ = trimTrailingWhitespace(expected)
               found_    = trimTrailingWhitespace(found)
            case (IGNORE_DIFFERENCES%value)
               expected_ = trimTrailingWhitespace(expected)
               found_    = trimTrailingWhitespace(found)
            case (KEEP_ALL%value)
               expected_ = expected
               found_    = found
            end select

            throwMessage = &
                 & 'String assertion failed:' // new_line('A') // &
                 & '    expected: <"' // expected_ // '">' // new_line('A') // &
                 & '   but found: <"' // found_ // '">' // new_line('A') // &
                 & '  first diff:   ' // repeat('-', numSameCharacters) // '^'

            call throw(appendWithSpace(message_, throwMessage), location)

         end if

      ! else ! if checkCharacterByCharacter == .false. and we don't have to compare character-by-character

      end if

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


#ifdef _REAL16_IEEE_SUPPORT
   subroutine assertIsNaN_REAL16(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL16), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(ieee_is_nan(x), message, location)
      
   end subroutine assertIsNaN_REAL16
#endif

#ifdef _REAL32_IEEE_SUPPORT
   subroutine assertIsNaN_REAL32(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL32), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(ieee_is_nan(x), message, location)
      
   end subroutine assertIsNaN_REAL32
#endif

#ifdef _REAL64_IEEE_SUPPORT
   subroutine assertIsNaN_REAL64(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL64), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(ieee_is_nan(x), message, location)
      
   end subroutine assertIsNaN_REAL64
#endif

#ifdef _REAL80_IEEE_SUPPORT
   subroutine assertIsNaN_REAL80(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL80), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(ieee_is_nan(x), message, location)
      
   end subroutine assertIsNaN_REAL80
#endif

#ifdef _REAL128_IEEE_SUPPORT
   subroutine assertIsNaN_REAL128(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL128), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(ieee_is_nan(x), message, location)
      
   end subroutine assertIsNaN_REAL128
#endif

#ifdef _REAL256_IEEE_SUPPORT
   subroutine assertIsNaN_REAL256(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL256), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(ieee_is_nan(x), message, location)
      
   end subroutine assertIsNaN_REAL256
#endif


#ifdef _REAL16_IEEE_SUPPORT
   subroutine assertIsNotNaN_real16(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL16), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertFalse(ieee_is_nan(x), message, location)

   end subroutine assertIsNotNaN_real16
#endif

#ifdef _REAL32_IEEE_SUPPORT
   subroutine assertIsNotNaN_real32(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL32), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertFalse(ieee_is_nan(x), message, location)

   end subroutine assertIsNotNaN_real32
#endif

#ifdef _REAL64_IEEE_SUPPORT
   subroutine assertIsNotNaN_real64(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL64), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertFalse(ieee_is_nan(x), message, location)

   end subroutine assertIsNotNaN_real64
#endif

#ifdef _REAL80_IEEE_SUPPORT
   subroutine assertIsNotNaN_real80(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL80), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertFalse(ieee_is_nan(x), message, location)

   end subroutine assertIsNotNaN_real80
#endif

#ifdef _REAL128_IEEE_SUPPORT
   subroutine assertIsNotNaN_real128(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL128), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertFalse(ieee_is_nan(x), message, location)

   end subroutine assertIsNotNaN_real128
#endif

#ifdef _REAL16_IEEE_SUPPORT
   subroutine assertIsNotNaN_real256(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
      real(kind=REAL256), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertFalse(ieee_is_nan(x), message, location)

   end subroutine assertIsNotNaN_real256
#endif


#ifdef _REAL16_IEEE_SUPPORT
   subroutine assertIsFinite_real16(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL16), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertTrue(ieee_is_finite(x), message, location)

   end subroutine assertIsFinite_real16
#endif

#ifdef _REAL32_IEEE_SUPPORT
   subroutine assertIsFinite_real32(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL32), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertTrue(ieee_is_finite(x), message, location)

   end subroutine assertIsFinite_real32
#endif

#ifdef _REAL64_IEEE_SUPPORT
   subroutine assertIsFinite_real64(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL64), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertTrue(ieee_is_finite(x), message, location)

   end subroutine assertIsFinite_real64
#endif

#ifdef _REAL80_IEEE_SUPPORT
   subroutine assertIsFinite_real80(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL80), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertTrue(ieee_is_finite(x), message, location)

   end subroutine assertIsFinite_real80
#endif

#ifdef _REAL128_IEEE_SUPPORT
   subroutine assertIsFinite_real128(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL128), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertTrue(ieee_is_finite(x), message, location)

   end subroutine assertIsFinite_real128
#endif

#ifdef _REAL256_IEEE_SUPPORT
   subroutine assertIsFinite_real256(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL256), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertTrue(ieee_is_finite(x), message, location)

   end subroutine assertIsFinite_real256
#endif
   

#ifdef _REAL16_IEEE_SUPPORT
   subroutine assertIsInfinite_real16(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL16), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertFalse(ieee_is_finite(x), message, location)
      
   end subroutine assertIsInfinite_real16
#endif

#ifdef _REAL32_IEEE_SUPPORT
   subroutine assertIsInfinite_real32(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL32), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertFalse(ieee_is_finite(x), message, location)
      
   end subroutine assertIsInfinite_real32
#endif

#ifdef _REAL64_IEEE_SUPPORT
   subroutine assertIsInfinite_real64(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL64), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertFalse(ieee_is_finite(x), message, location)
      
   end subroutine assertIsInfinite_real64
#endif

#ifdef _REAL80_IEEE_SUPPORT
   subroutine assertIsInfinite_real80(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL80), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertFalse(ieee_is_finite(x), message, location)
      
   end subroutine assertIsInfinite_real80
#endif

#ifdef _REAL128_IEEE_SUPPORT
   subroutine assertIsInfinite_real128(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL128), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertFalse(ieee_is_finite(x), message, location)
      
   end subroutine assertIsInfinite_real128
#endif

#ifdef _REAL256_IEEE_SUPPORT
   subroutine assertIsInfinite_real256(x, message, location)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      real(kind=REAL256), intent(in) :: x
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      
      call assertFalse(ieee_is_finite(x), message, location)
      
   end subroutine assertIsInfinite_real256
#endif


end module PF_AssertBasic
