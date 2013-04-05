module AssertInteger_mod
   use Exception_mod
   implicit none
   private

   public :: assertEqual

   interface assertEqual
      module procedure assertEqualIntegerScalar
   end interface assertEqual

contains

   subroutine assertEqualIntegerScalar(expected, found)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      integer, intent(in) :: expected
      integer, intent(in) :: found

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if (expected /= found) then
         write(throwMessage,'((a,a),(a,i0,a,a),(a,i0,a))') 'Integer scalar assertion failed:', new_line('A'), &
              & '    expected: <', expected, '>', new_line('A'), &
              & '   but found: <', found, '>'
         call throw(throwMessage)
      end if
   end subroutine assertEqualIntegerScalar

end module AssertInteger_mod
