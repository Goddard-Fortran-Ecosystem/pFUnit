module AssertInteger_mod
   use ASsertBasic_mod
   use Exception_mod
   implicit none
   private

   public :: assertEqual

   interface assertEqual
      module procedure assertEqualIntegerScalar
      module procedure assertEqualInteger1D1D
      module procedure assertEqualInteger0D1D
      module procedure assertEqualInteger2D2D
   end interface assertEqual

contains

   subroutine assertEqualIntegerScalar(expected, found)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if (expected /= found) then
         write(throwMessage,'(a,i0,a,i0,a)') &
              & 'expected: <', expected, '> but found: <', found, '>'
         call throw(throwMessage)
      end if
   end subroutine assertEqualIntegerScalar

   subroutine assertEqualInteger1D1D(expected, found)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:)
      integer, intent(in) :: found(:)

      integer :: i
      character(len=MAXLEN_MESSAGE) :: message

      if (nonConformable(shape(expected), shape(found))) then
         call throw('nonconforming arrays - expected shape: ' // &
              & trim(toString(shape(expected))) // ' but found shape: ' // &
              & trim(toString(shape(found))))
         return
      end if

      do i = 1, size(expected)
         if (found(i) /= expected(i)) then
            write(message,'(a,i0,a,i0,a,i0,a)') 'expected: <',expected(i),'> but found: <',found(i), &
                 & '> at position: [',i,']'
            call throw(message)
            exit
         end if
      end do

   end subroutine assertEqualInteger1D1D

   subroutine assertEqualInteger0D1D(expected, found) ! always conformable
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found(:)

      character(len=MAXLEN_MESSAGE) :: message
      integer :: i

      do i = 1, size(found)
         if (found(i) /= expected) then
            write(message,'(a,i0,a,i0,a,i0,a)') 'expected: <',expected,'> but found: <',found(i), &
                 & '> at position: [',i,']'
            call throw(message)
            exit
         end if
      end do

   end subroutine assertEqualInteger0D1D

   subroutine assertEqualInteger2D2D(expected, found)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected(:,:)
      integer, intent(in) :: found(:,:)

      integer :: i
      character(len=MAXLEN_MESSAGE) :: message
      integer :: loc(2)

      if (nonConformable(shape(expected), shape(found))) then
         call throw('nonconforming arrays - expected shape: ' // &
              & trim(toString(shape(expected))) // ' but found shape: ' // &
              & trim(toString(shape(found))))
         return
      end if

      if (any(found /= expected)) then
         loc = locationOfFirstDifference(found, expected)
         loc = maxloc(max(1, abs(found-expected))) ! will be the first loc in element order
         write(message,'(a,i0,a,i0,a,a)') 'expected: <',expected(loc(1),loc(2)), &
              & '> but found: <',found(loc(1),loc(2)), &
              & '> at position: ', toString(loc)
            call throw(message)
         end if

   contains

      function locationOfFirstDifference(a, b) result(location)
         integer, intent(in) :: a(:,:)
         integer, intent(in) :: b(:,:)
         
         integer :: location(2)

         location = maxloc(min(1,abs(a-b)))
      end function locationOfFirstDifference

   end subroutine assertEqualInteger2D2D


end module AssertInteger_mod
