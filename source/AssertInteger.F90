!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: AssertInteger
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
module AssertInteger_mod
   use AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   use StringConversionUtilities_mod
   implicit none
   private

   public :: assertEqual
   public :: assertLessThan
   public :: assertLessThanOrEqual
   public :: assertGreaterThan
   public :: assertGreaterThanOrEqual

   interface assertEqual
      module procedure assertEqualIntegerScalar_
      module procedure assertEqualInteger1D1D_
      module procedure assertEqualInteger0D1D_
      module procedure assertEqualInteger0D2D_
      module procedure assertEqualInteger2D2D_
   end interface assertEqual

   interface assertLessThan
      module procedure assertLessThan_
   end interface assertLessThan

   interface assertLessThanOrEqual
      module procedure assertLessThanOrEqual_
   end interface assertLessThanOrEqual

   interface assertGreaterThan
      module procedure assertGreaterThan_
   end interface assertGreaterThan

   interface assertGreaterThanOrEqual
      module procedure assertGreaterThanOrEqual_
   end interface assertGreaterThanOrEqual

   interface locationOfFirstNonzero
      module procedure locationOfFirstNonzero_1d
      module procedure locationOfFirstNonzero_2d
   end interface locationOfFirstNonzero

contains

   subroutine assertEqualIntegerScalar_(expected, found, message, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if (expected /= found) then
         write(throwMessage,'(a,i0,a,i0,a)') &
              & 'expected: <', expected, '> but found: <', found, '>'
         call throw(appendWithSpace(message, throwMessage), &
              & location)
      end if

   end subroutine assertEqualIntegerScalar_

   subroutine assertEqualInteger1D1D_(expected, found, message, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:)
      integer, intent(in) :: found(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(1)

      call AssertSameShape(shape(expected), shape(found), message, location)
      if (anyExceptions()) return

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(expected - found)
         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected(loc(1)),'> but found: <',found(loc(1)), &
              & '> at position: ',toString(loc)
         call throw(appendWithSpace(message, throwMessage), &
              & location)
      end if

   end subroutine assertEqualInteger1D1D_

   subroutine assertEqualInteger0D1D_(expected, found, message, location) ! always conformable
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_


      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(1)

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(found - expected)
         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected,'> but found: <',found(loc(1)), &
              & '> at position: ',toString(loc)
         call throw(appendWithSpace(message, throwMessage), &
              & location)
      end if

   end subroutine assertEqualInteger0D1D_

   subroutine assertEqualInteger2D2D_(expected, found, message, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:,:)
      integer, intent(in) :: found(:,:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(2)

      call AssertSameShape(shape(expected), shape(found), message, location)
      if (anyExceptions()) return

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(found - expected)

         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected(loc(1),loc(2)), &
              & '> but found: <',found(loc(1),loc(2)), &
              & '> at position: ', toString(loc)
         call throw(appendWithSpace(message, throwMessage), &
              & location)
      end if

   end subroutine assertEqualInteger2D2D_

   subroutine assertEqualInteger0D2D_(expected, found, message, location) ! always conformable
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found(:,:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(2)

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(found - expected)
         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected,'> but found: <',found(loc(1),loc(2)), &
              & '> at position: ',toString(loc)
         call throw(appendWithSpace(message, throwMessage), location)
      end if

   end subroutine assertEqualInteger0D2D_

   subroutine assertLessThan_(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a < b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be less than: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), location)

      end if

   end subroutine assertLessThan_

   subroutine assertLessThanOrEqual_(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a <= b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be less than or equal to: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), &
              & location)

      end if

   end subroutine assertLessThanOrEqual_

   subroutine assertGreaterThan_(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a > b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be greater than: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), location)

      end if

   end subroutine assertGreaterThan_

   subroutine assertGreaterThanOrEqual_(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
!      type (SourceLocation) :: location_

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a >= b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be greater than or equal to: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), location)

      end if

   end subroutine assertGreaterThanOrEqual_

   function locationOfFirstNonzero_1d(array) result(loc)
      integer, intent(in) :: array(:)
      integer :: loc(1)

      loc = maxloc(min(1, abs(array)))

   end function locationOfFirstNonzero_1d

   function locationOfFirstNonzero_2d(array) result(loc)
      integer, intent(in) :: array(:,:)
      integer :: loc(2)

      loc = maxloc(min(1, abs(array)))

   end function locationOfFirstNonzero_2d

end module AssertInteger_mod
