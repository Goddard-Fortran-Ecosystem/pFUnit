module pf_StringDescription_mod
   use pf_MatcherDescription_mod
   implicit none
   private

   public :: StringDescription

   type, extends(MatcherDescription) :: StringDescription
      private
      character(:), allocatable :: out
   contains
      procedure :: append_text 
      procedure :: append_description_of
      procedure :: to_string
   end type StringDescription


contains


   subroutine append_text(this, text)
      class (StringDescription), intent(inout) :: this
      character(*), intent(in) :: text

      this%out = this%out // text
   end subroutine append_text

   subroutine append_description_of(this, value)
      class (StringDescription), intent(inout) :: this
      class (SelfDescribing), intent(in) :: value

      call value%describe_to(this)
      
   end subroutine append_description_of


   function to_string(this) result(string)
      character(:), allocatable :: string
      class (StringDescription), intent(in) :: this

      string = this%out

   end function to_string

end module pf_StringDescription_mod
