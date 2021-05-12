#include "unused_dummy.fh"

module pf_AssertUtilities
   use pf_KeywordEnforcer
   use pf_SourceLocation
   use pf_StringUtilities
   use pf_ExceptionList
   implicit none
   private

   public :: conformable
   public :: fail_generic
   public :: fail_not_conformable
   public :: fail_not_equal
   public :: fail_not_equivalent
   public :: fail_equal
   public :: fail_equivalent
   public :: fail_not_associated
   public :: fail_not_less_than
   public :: fail_not_less_than_or_equal
   public :: fail_not_greater_than
   public :: fail_not_greater_than_or_equal
   public :: fail_not_relatively_equal
   public :: fail_not_relatively_min_equal

contains

   
   logical function conformable(shape_A, shape_B)
      integer, intent(in) :: shape_A(:)
      integer, intent(in) :: shape_B(:)

      integer :: rank_A
      integer :: rank_B

      rank_A = size(shape_A)
      rank_B = size(shape_B)

      conformable = .false. ! unless ...

      ! Scalars are always conforable with anything
      conformable = (rank_A == 0) .or. (rank_B == 0)
      if (conformable) return

      ! Same rank?
      conformable = (rank_A == rank_B)
      if (.not. conformable) return

      ! Same shape?
      conformable = all(shape_A == shape_B)

   end function conformable


   subroutine fail_generic(fail_message, unused, message, location)
      ! Positional arguments
      character(*), intent(in) :: fail_message
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Begin keyword arguments
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      _UNUSED_DUMMY(unused)

      call throw(base_message(fail_message, message), location=location)

   end subroutine fail_generic

   subroutine fail_not_conformable(shape_expected, shape_actual, unused, message, location)
      ! Positional arguments
      integer, intent(in) :: shape_expected(:)
      integer, intent(in) :: shape_actual(:)
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Begin keyword arguments
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('Arrays not conformable', message)
      fail_message = fail_message // new_line('A') // '    Expected shape: ' // toString(shape_expected)
      fail_message = fail_message // new_line('A') // '      Actual shape: ' // toString(shape_actual)

      call throw(fail_message, location)
      
   end subroutine fail_not_conformable


   subroutine fail_not_equal(expected, actual, difference, unused, index, message, location)
      character(*), intent(in) :: expected
      character(*), intent(in) :: actual
      character(*), intent(in) :: difference
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertEqual', message, index)
      fail_message = fail_message // new_line('A')    // '      Expected: <' // expected // '>' 
      fail_message = fail_message // new_line('A')    // '        Actual: <' // actual // '>'
      fail_message = fail_message // new_line('A')    // '    Difference: ' // difference
      if (present(index)) then
         fail_message = fail_message // new_line('A') // '      at index: ' // toString(index)
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_equal
   
   
   subroutine fail_not_equivalent(expected, actual, unused, index, message, location)
      character(*), intent(in) :: expected
      character(*), intent(in) :: actual
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertEquivalent', message, index)
      fail_message = fail_message // new_line('A') // '    Expected: <' // expected // '>' 
      fail_message = fail_message // new_line('A') // '    Actual:   <' // actual // '>'
      if (present(index)) then
         fail_message = fail_message // new_line('A') // '    at index: ' // toString(index) // '>'
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_equivalent
   

   subroutine fail_equivalent(expected, actual, unused, index, message, location)
      character(*), intent(in) :: expected
      character(*), intent(in) :: actual
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertNotEquivalent', message, index)
      fail_message = fail_message // new_line('A') // '    Expected: <' // expected // '>' 
      fail_message = fail_message // new_line('A') // '    Actual:   <' // actual // '>'
      if (present(index)) then
         fail_message = fail_message // new_line('A') // '    at index: ' // toString(index) // '>'
      end if

      call throw(fail_message, location)
      
   end subroutine fail_equivalent
   

   subroutine fail_equal(actual, unused, difference, index, message, location)
      character(*), intent(in) :: actual
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      character(*), optional, intent(in) :: difference
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertNotEqual', message, index)
      fail_message = fail_message // new_line('A') //    '    Same value: <' // actual // '>'
      if (present(difference)) then
         fail_message = fail_message // new_line('A') // '    Difference: ' // difference
      end if
      if (present(index)) then
         fail_message = fail_message // new_line('A') // '      at index: ' // toString(index)
      end if

      call throw(fail_message, location)

   end subroutine fail_equal
   

   subroutine fail_not_associated(unused, message, location)
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertAssociated', message)

      call throw(fail_message, location)

   end subroutine fail_not_associated
   


   subroutine fail_not_less_than(lhs, rhs, unused, index, message, location)
      character(*), intent(in) :: lhs
      character(*), intent(in) :: rhs
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertLessThan', message, index)
      fail_message = fail_message // new_line('A') //     '              LHS: <' // lhs // '>'
      fail_message = fail_message // new_line('A') //     '    not less than: <' // rhs // '>'
      if (present(index)) then
         fail_message = fail_message  // new_line('A') // '         at index: ' // toString(index)
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_less_than
   
   
   subroutine fail_not_less_than_or_equal(lhs, rhs, unused, index, message, location)
      character(*), intent(in) :: lhs
      character(*), intent(in) :: rhs
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertLessThanOrEqual', message, index)
      fail_message = fail_message // new_line('A') //     '                          LHS: <' // lhs // '>'
      fail_message = fail_message // new_line('A') //     '    not less than or equal to: <' // rhs // '>'
      if (present(index)) then
         fail_message = fail_message  // new_line('A') // '                     at index: ' // toString(index)
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_less_than_or_equal
   
   subroutine fail_not_greater_than(lhs, rhs, unused, index, message, location)
      character(*), intent(in) :: lhs
      character(*), intent(in) :: rhs
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertGreaterThan', message, index)
      fail_message = fail_message // new_line('A') //     '                 LHS: <' // lhs // '>'
      fail_message = fail_message // new_line('A') //     '    not greater than: <' // rhs // '>'
      if (present(index)) then
         fail_message = fail_message  // new_line('A') // '            at index: ' // toString(index)
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_greater_than
   
   
   subroutine fail_not_greater_than_or_equal(lhs, rhs, unused, index, message, location)
      character(*), intent(in) :: lhs
      character(*), intent(in) :: rhs
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertGreaterThanOrEqual', message, index)
      fail_message = fail_message // new_line('A') //     '                             LHS: <' // lhs // '>'
      fail_message = fail_message // new_line('A') //     '    not greater than or equal to: <' // rhs // '>'
      if (present(index)) then
         fail_message = fail_message  // new_line('A') // '                        at index: ' // toString(index)
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_greater_than_or_equal


   subroutine fail_not_relatively_equal(expected, actual, difference, unused, index, message, location)
      character(*), intent(in) :: expected
      character(*), intent(in) :: actual
      character(*), intent(in) :: difference
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('AssertRelativelyEqual', message, index)
      fail_message = fail_message // new_line('A')    // '           Expected: <' // expected // '>' 
      fail_message = fail_message // new_line('A')    // '             Actual: <' // actual // '>'
      fail_message = fail_message // new_line('A')    // '    Rel. difference: ' // difference
      if (present(index)) then
         fail_message = fail_message // new_line('A') // '      at index: ' // toString(index)
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_relatively_equal

   subroutine fail_not_relatively_min_equal(expected, actual, difference, unused, index, message, location)
      character(*), intent(in) :: expected
      character(*), intent(in) :: actual
      character(*), intent(in) :: difference
      ! Separator
      class (KeywordEnforcer), optional, intent(in) :: unused
      ! Keyword arguments
      integer, optional, intent(in) :: index(:)
      character(*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: fail_message

      _UNUSED_DUMMY(unused)

      fail_message = base_message('assertRelMinEqual', message, index)
      fail_message = fail_message // new_line('A')    // '           Expected: <' // expected // '>' 
      fail_message = fail_message // new_line('A')    // '             Actual: <' // actual // '>'
      fail_message = fail_message // new_line('A')    // '    Rel. difference: ' // difference
      if (present(index)) then
         fail_message = fail_message // new_line('A') // '      at index: ' // toString(index)
      end if

      call throw(fail_message, location)
      
   end subroutine fail_not_relatively_min_equal

   function base_message(failure_type, user_message, index) result(message)
      character(:), allocatable :: message
      character(*), intent(in) :: failure_type
      character(*), optional, intent(in) :: user_message
      integer, optional, intent(in) :: index(:)

      if (present(user_message)) then
         message = user_message // new_line('A')
      else
         message = ''
      end if

      if (present(index)) then
         message = message // 'Array'
      end if

      message = message // failure_type // ' failure:'
      
   end function base_message
     
   
end module pf_AssertUtilities
