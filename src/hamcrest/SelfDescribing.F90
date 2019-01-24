module pf_SelfDescribing
  use pf_SurrogateDescription
  implicit none
  private

  public :: SelfDescribing

  type, abstract :: SelfDescribing
     private
     character(:), allocatable :: type_name
   contains
     procedure(describe_to), deferred :: type_unsafe_describe_to
     procedure :: get_type_name
     procedure :: set_type_name
  end type SelfDescribing

   abstract interface

      subroutine describe_to(this, description)
         import SelfDescribing
         import SurrogateDescription
         class (SelfDescribing), intent(in) :: this
         class (SurrogateDescription), intent(inout) :: description
      end subroutine describe_to

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
   
end module pf_SelfDescribing
