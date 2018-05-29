module pf_AbstractMatcher_mod
   use pf_MatcherDescription_mod, only: SelfDescribing
   implicit none
   private

   public :: AbstractMatcher

   type, abstract, extends(SelfDescribing) :: AbstractMatcher
   contains
      procedure(matches), deferred :: matches
      procedure(describe_mismatch), deferred :: describe_mismatch
   end type AbstractMatcher

   abstract interface

      logical function matches(this, actual)
         import AbstractMatcher
         class (AbstractMatcher), intent(in) :: this
         class(*), intent(in) :: actual
      end function matches

      subroutine describe_mismatch(this, actual, description)
         use pf_MatcherDescription_mod, only: MatcherDescription
         import AbstractMatcher
         class (AbstractMatcher), intent(in) :: this
         class (*), intent(in) :: actual
         class (MatcherDescription), intent(inout) :: description
      end subroutine describe_mismatch
   end interface

end module pf_AbstractMatcher_mod
   
