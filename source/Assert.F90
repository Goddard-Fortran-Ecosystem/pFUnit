module Assert_mod
   use AssertBasic_mod
   use AssertInteger_mod
   implicit none
   private

   public :: assertTrue
   public :: assertFalse
   public :: assertEqual
   public :: assertExceptionRaised

   interface assertEqual
      module procedure assertEqualString
   end interface

contains

   subroutine assertFalse(condition)
      logical, intent(in) :: condition
      call assertTrue(.not. condition)
   end subroutine assertFalse

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

end module Assert_mod
