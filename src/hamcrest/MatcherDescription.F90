module pf_MatcherDescription
  use pf_SurrogateDescription
  use pf_SelfDescribing
  use pf_SelfDescribingVector
  implicit none
  private

   public :: MatcherDescription

   type, abstract, extends(SurrogateDescription) :: MatcherDescription
   contains
      procedure(append_text), deferred :: append_text
      procedure(append_description_of), deferred :: append_description_of ! selfd
      procedure(to_string), deferred :: to_string
      procedure(append_value_scalar), deferred :: append_value_scalar
      generic :: append_value => append_value_scalar
      procedure(append_list_array), deferred :: append_list_array
      procedure(append_list_vector), deferred :: append_list_vector
      generic :: append_list => append_list_array
      generic :: append_list => append_list_vector
   end type MatcherDescription


   abstract interface

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


      subroutine append_value_scalar(this, value)
        import MatcherDescription
        class(MatcherDescription), intent(inout) :: this
        class(*), intent(in) :: value
      end subroutine append_value_scalar

      function to_string(this) result(string)
         import SelfDescribing
         import MatcherDescription
         character(:), allocatable :: string
         class (MatcherDescription), intent(in) :: this
      end function to_string

      subroutine append_list_array(this, start, separator, end, values)
        import MatcherDescription
        import SelfDescribing
        class(MatcherDescription), intent(inout) :: this
        character(*), intent(in) :: start
        character(*), intent(in) :: separator
        character(*), intent(in) :: end
        class(SelfDescribing), intent(in) :: values(:)
      end subroutine append_list_array

      subroutine append_list_vector(this, start, separator, end, values)
        import MatcherDescription
        import SelfDescribingVector
        class(MatcherDescription), intent(inout) :: this
        character(*), intent(in) :: start
        character(*), intent(in) :: separator
        character(*), intent(in) :: end
        class(SelfDescribingVector), intent(in) :: values
      end subroutine append_list_vector

   end interface


end module pf_MatcherDescription
