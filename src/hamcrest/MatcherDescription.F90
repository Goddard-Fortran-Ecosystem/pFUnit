module pf_MatcherDescription_mod
   implicit none
   private

   public :: MatcherDescription
   public :: SelfDescribing  ! Same module to workaround for forward reference

   type, abstract :: SelfDescribing
   contains
      procedure(describe_to), deferred :: describe_to
   end type SelfDescribing


   type, abstract :: MatcherDescription
   contains
      procedure(append_text), deferred :: append_text
      procedure(append_description_of), deferred :: append_description_of ! selfd
      procedure(to_string), deferred :: to_string
   end type MatcherDescription


   abstract interface

      subroutine describe_to(this, description)
         import SelfDescribing
         import MatcherDescription
         class (SelfDescribing), intent(in) :: this
         class (MatcherDescription), intent(inout) :: description
      end subroutine describe_to


      subroutine append_text(this, text)
         import SelfDescribing
         import MatcherDescription
         class (MatcherDescription), intent(inout) :: this
         character(*), intent(in) :: text
      end subroutine append_text

      subroutine append_description_of(this, value)
         import SelfDescribing
         import MatcherDescription
         class (MatcherDescription), intent(inout) :: this
         class (SelfDescribing), intent(in) :: value
      end subroutine append_description_of

      function to_string(this) result(string)
         import SelfDescribing
         import MatcherDescription
         character(:), allocatable :: string
         class (MatcherDescription), intent(in) :: this
      end function to_string

   end interface

end module pf_MatcherDescription_mod
