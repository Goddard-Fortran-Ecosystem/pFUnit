module Assert_mod
   implicit none
   private

   public :: assertTrue
   public :: assertFalse
   public :: assertEqual

   interface assertEqual
      module procedure assertEqualIntegerScalar
      module procedure assertEqualString
   end interface

   interface assertTrue
      module procedure assertTrueBasic
      module procedure assertTrueLineNumber
      module procedure assertTrueMessage
   end interface

contains

   subroutine assertTrueBasic(condition)
      use Exception_mod, only: throw
      logical, intent(in) :: condition
      if (.not. condition) call throw('Logical assertion failed.')
   end subroutine assertTrueBasic

   subroutine assertTrueLineNumber(condition, lineNumber)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      logical, intent(in) :: condition
      integer, intent(in) :: lineNumber
      character(len=MAXLEN_MESSAGE) :: message

      if (.not. condition) then
         write(message,'("Logical assertion failed at line <",i0,">")') lineNumber
         call throw(message)
      end if
   end subroutine assertTrueLineNumber

   subroutine assertTrueMessage(condition, message)
      use Exception_mod, only: throw
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (.not. condition) call throw('Logical assertion failed :: '//trim(message))
   end subroutine assertTrueMessage

   subroutine assertFalse(condition)
      logical, intent(in) :: condition
      call assertTrue(.not. condition)
   end subroutine assertFalse

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
