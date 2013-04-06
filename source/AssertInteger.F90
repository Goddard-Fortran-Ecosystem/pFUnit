module AssertInteger_mod
   use AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   implicit none
   private

   public :: assertEqual
   public :: assertLessThan
   public :: assertLessThanOrEqual
   public :: assertGreaterThan
   public :: assertGreaterThanOrEqual

   interface assertEqual
      module procedure assertEqualIntegerScalar
      module procedure assertEqualIntegerScalar_withMessage
      module procedure assertEqualInteger1D1D
      module procedure assertEqualInteger1D1D_withMessage
      module procedure assertEqualInteger0D1D
      module procedure assertEqualInteger0D1D_withMessage
      module procedure assertEqualInteger0D2D
      module procedure assertEqualInteger0D2D_withMessage
      module procedure assertEqualInteger2D2D
      module procedure assertEqualInteger2D2D_withMessage
   end interface assertEqual

   interface assertLessThan
      module procedure assertLessThan_
      module procedure assertLessThan_withMessage
   end interface assertLessThan

   interface assertLessThanOrEqual
      module procedure assertLessThanOrEqual_
      module procedure assertLessThanOrEqual_withMessage
   end interface assertLessThanOrEqual

   interface assertGreaterThan
      module procedure assertGreaterThan_
      module procedure assertGreaterThan_withMessage
   end interface assertGreaterThan

   interface assertGreaterThanOrEqual
      module procedure assertGreaterThanOrEqual_
      module procedure assertGreaterThanOrEqual_withMessage
   end interface assertGreaterThanOrEqual

   interface locationOfFirstNonzero
      module procedure locationOfFirstNonzero_1d
      module procedure locationOfFirstNonzero_2d
   end interface locationOfFirstNonzero

contains

   subroutine assertEqualIntegerScalar(expected, found, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found
      type (SourceLocation), optional, intent(in) :: location

      call assertEqual(expected, found, NULL_MESSAGE, location)

   end subroutine assertEqualIntegerScalar

   subroutine assertEqualIntegerScalar_withMessage(expected, found, message, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if (expected /= found) then
         write(throwMessage,'(a,i0,a,i0,a)') &
              & 'expected: <', expected, '> but found: <', found, '>'
         call throw(appendWithSpace(message,throwMessage), location)
      end if
   end subroutine assertEqualIntegerScalar_withMessage

   subroutine assertEqualInteger1D1D(expected, found, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:)
      integer, intent(in) :: found(:)
      type (SourceLocation), optional, intent(in) :: location

      call assertEqual(expected, found, NULL_MESSAGE, location)

   end subroutine assertEqualInteger1D1D

   subroutine assertEqualInteger1D1D_withMessage(expected, found, message, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:)
      integer, intent(in) :: found(:)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(1)

      call AssertSameShape(shape(expected), shape(found), message, location)
      if (anyExceptions()) return

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(expected - found)
         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected(loc(1)),'> but found: <',found(loc(1)), &
              & '> at position: ',toString(loc)
         call throw(appendWithSpace(message, throwMessage))
      end if

   end subroutine assertEqualInteger1D1D_withMessage

   subroutine assertEqualInteger0D1D(expected, found, location) ! always conformable
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found(:)
      type (SourceLocation), optional, intent(in) :: location

      call assertEqual(expected, found, NULL_MESSAGE, location)
   end subroutine assertEqualInteger0D1D

   subroutine assertEqualInteger0D1D_withMessage(expected, found, message, location) ! always conformable
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found(:)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(1)

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(found - expected)
         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected,'> but found: <',found(loc(1)), &
              & '> at position: ',toString(loc)
         call throw(appendWithSpace(message, throwMessage), location)
      end if

   end subroutine assertEqualInteger0D1D_withMessage

   subroutine assertEqualInteger2D2D(expected, found, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:,:)
      integer, intent(in) :: found(:,:)
      type (SourceLocation), optional, intent(in) :: location

      call assertEqual(expected, found, NULL_MESSAGE, location)
   end subroutine assertEqualInteger2D2D

   subroutine assertEqualInteger2D2D_withMessage(expected, found, message, location)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:,:)
      integer, intent(in) :: found(:,:)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(2)

      call AssertSameShape(shape(expected), shape(found), message, location)
      if (anyExceptions()) return

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(found - expected)

         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected(loc(1),loc(2)), &
              & '> but found: <',found(loc(1),loc(2)), &
              & '> at position: ', toString(loc)
         call throw(appendWithSpace(message, throwMessage), location)
      end if

   end subroutine assertEqualInteger2D2D_withMessage

   subroutine assertEqualInteger0D2D(expected, found, location) ! always conformable
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found(:,:)
      type (SourceLocation), optional, intent(in) :: location

      call assertEqual(expected, found, NULL_MESSAGE, location)
   end subroutine assertEqualInteger0D2D

   subroutine assertEqualInteger0D2D_withMessage(expected, found, message, location) ! always conformable
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found(:,:)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: loc(2)

      if (any(found /= expected)) then
         loc = locationOfFirstNonzero(found - expected)
         write(throwMessage,'(a,i0,a,i0,a,a)') 'expected: <',expected,'> but found: <',found(loc(1),loc(2)), &
              & '> at position: ',toString(loc)
         call throw(appendWithSpace(message, throwMessage), location)
      end if

   end subroutine assertEqualInteger0D2D_withMessage

   subroutine assertLessThan_(a, b, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      type (SourceLocation), optional, intent(in) :: location
      
      call assertLessThan(a, b, NULL_MESSAGE, location)

   end subroutine assertLessThan_

   subroutine assertLessThan_withMessage(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a < b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be less than: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), location)

      end if

   end subroutine assertLessThan_withMessage

   subroutine assertLessThanOrEqual_(a, b, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      type (SourceLocation), optional, intent(in) :: location
      
      call assertLessThanOrEqual(a, b, NULL_MESSAGE, location)

   end subroutine assertLessThanOrEqual_

   subroutine assertLessThanOrEqual_withMessage(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a <= b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be less than or equal to: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), location)

      end if

   end subroutine assertLessThanOrEqual_withMessage

   subroutine assertGreaterThan_(a, b, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      type (SourceLocation), optional, intent(in) :: location
      
      call assertGreaterThan(a, b, NULL_MESSAGE, location)

   end subroutine assertGreaterThan_

   subroutine assertGreaterThan_withMessage(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a > b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be greater than: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), location)

      end if

   end subroutine assertGreaterThan_withMessage

   subroutine assertGreaterThanOrEqual_(a, b, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      type (SourceLocation), optional, intent(in) :: location
      
      call assertGreaterThanOrEqual(a, b, NULL_MESSAGE, location)

   end subroutine assertGreaterThanOrEqual_

   subroutine assertGreaterThanOrEqual_withMessage(a, b, message, location)
      integer, intent(in) :: a
      integer, intent(in) :: b
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if ( .not. (a >= b) ) then

         write(throwMessage,'(a,i0,a,i0,a)') 'expected: <',a,'> to be greater than or equal to: <',b,'>'
         call throw(appendWithSpace(message,throwMessage), location)

      end if

   end subroutine assertGreaterThanOrEqual_withMessage

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
