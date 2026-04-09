module pf_GlobFilter
   use pf_TestFilter
   use pf_Test
   use PF_GlobPattern
   implicit none
   private

   public :: GlobFilter

   type, extends(TestFilter) :: GlobFilter
      private
      type(GlobPattern) :: pattern
    contains
      procedure :: filter
   end type GlobFilter


   interface GlobFilter
      module procedure new_GlobFilter
   end interface GlobFilter

contains

   function new_GlobFilter(pattern_str) result(filter)
     type(GlobFilter) :: filter
     character(*), intent(in) :: pattern_str

     filter%pattern = GlobPattern(pattern_str)
   end function new_GlobFilter


   logical function filter(this, a_test)
     class(GlobFilter), intent(in) :: this
     class(Test), intent(in) :: a_test

     filter = this%pattern%match(a_test%getName())
   end function filter


end module pf_GlobFilter
