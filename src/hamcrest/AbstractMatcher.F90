module pf_AbstractMatcher
   use pf_TypeSafeSelfDescribing
   implicit none
   private

   public :: AbstractMatcher

   type, abstract, extends(TypeSafeSelfDescribing) :: AbstractMatcher
   contains
      procedure(matches), deferred :: matches
      procedure(describe_mismatch), deferred :: describe_mismatch
   end type AbstractMatcher

   abstract interface

      logical function matches(this, actual_value)
         import AbstractMatcher
         class (AbstractMatcher), intent(in) :: this
         class(*), intent(in) :: actual_value
      end function matches

      subroutine describe_mismatch(this, actual, description)
         use pf_MatcherDescription, only: MatcherDescription
         import AbstractMatcher
         class (AbstractMatcher), intent(in) :: this
         class (*), intent(in) :: actual
         class (MatcherDescription), intent(inout) :: description
      end subroutine describe_mismatch
   end interface

 end module pf_AbstractMatcher
