#ifndef _WIN32
module pf_RegexFilter
   use pf_TestFilter
   use pf_Test
   use pf_regex_module
   implicit none
   private

   public :: RegexFilter

   type, extends(TestFilter) :: RegexFilter
      private
      type(regex_type) :: regex
      logical :: compiled = .false.
      character(:), allocatable :: pattern
    contains
      procedure :: filter
      final :: cleanup
   end type RegexFilter


   interface RegexFilter
      module procedure new_RegexFilter
   end interface RegexFilter

contains

   function new_RegexFilter(pattern) result(filter)
     type(RegexFilter) :: filter
     character(*), intent(in) :: pattern
     integer :: status

     filter%pattern = pattern
     
     ! Compile regex with 'x' (extended) flag for full regex syntax
     call regcomp(filter%regex, pattern, flags='x', status=status)
     
     if (status == 0) then
        filter%compiled = .true.
     else
        ! Compilation failed - filter will never match
        filter%compiled = .false.
        ! Could optionally print warning here
     end if
   end function new_RegexFilter


   logical function filter(this, a_test)
     class(RegexFilter), intent(in) :: this
     class(Test), intent(in) :: a_test

     if (.not. this%compiled) then
        filter = .false.
        return
     end if
     
     filter = regexec(this%regex, a_test%getName())
   end function filter


   subroutine cleanup(this)
     type(RegexFilter), intent(inout) :: this
     
     if (this%compiled) then
        call regfree(this%regex)
     end if
   end subroutine cleanup


end module pf_RegexFilter
#endif
