module pf_MatcherDescription_mod
   implicit none
   private

   public :: MatcherDescription
   public :: SelfDescribing  ! Same module to workaround for forward reference

   type, abstract :: SelfDescribing
      private
      character(:), allocatable :: type_name
   contains
      procedure(describe_to), deferred :: describe_to
      procedure :: get_type_name
      procedure :: set_type_name
   end type SelfDescribing


   type, abstract :: MatcherDescription
   contains
      procedure(append_text), deferred :: append_text
      procedure(append_description_of), deferred :: append_description_of ! selfd
      procedure(to_string), deferred :: to_string
      procedure(append_value_scalar), deferred :: append_value_scalar
      generic :: append_value => append_value_scalar
      procedure(append_list), deferred :: append_list
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

      subroutine append_list(this, start, separator, end, values)
        import MatcherDescription
        import SelfDescribing
        class(MatcherDescription), intent(inout) :: this
        character(*), intent(in) :: start
        character(*), intent(in) :: separator
        character(*), intent(in) :: end
        class(SelfDescribing), intent(in) :: values(:)
      end subroutine append_list
   end interface

 contains

   function get_type_name(this) result(type_name)
     character(:), allocatable :: type_name
     class(SelfDescribing), intent(in) :: this
     if (.not. allocated(this%type_name)) then
        type_name = "UNKNOWN_TYPE"
     else
        type_name = this%type_name
     end if
   end function get_type_name


   subroutine set_type_name(this, type_name)
     class(SelfDescribing), intent(inout) :: this
     character(*), intent(in) :: type_name
     this%type_name = type_name
   end subroutine set_type_name
   

end module pf_MatcherDescription_mod
