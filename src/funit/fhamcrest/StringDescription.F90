module pf_StringDescription
   use pf_MatcherDescription
   use pf_BaseDescription
   use pf_SelfDescribing
   implicit none
   private

   public :: StringDescription

   type, extends(BaseDescription) :: StringDescription
      private
      character(:), allocatable :: out
    contains
      procedure :: append_string
      procedure :: append_character
      procedure :: append_description_of
      procedure :: to_string
   end type StringDescription


contains


   subroutine append_string(this, text)
      class (StringDescription), intent(inout) :: this
      character(*), intent(in) :: text

      if (.not. allocated(this%out)) then
         this%out = ''
      end if

      this%out = this%out // text
    end subroutine append_string

   subroutine append_character(this, char)
      class (StringDescription), intent(inout) :: this
      character(len=1), intent(in) :: char(1)

      this%out = this%out // char(1)
    end subroutine append_character


   recursive subroutine append_description_of(this, value)
      class (StringDescription), intent(inout) :: this
      class (SelfDescribing), intent(in) :: value

      call value%type_unsafe_describe_to(this)
      
   end subroutine append_description_of


   function to_string(this) result(string)
      character(:), allocatable :: string
      class (StringDescription), intent(in) :: this

      string = this%out

   end function to_string

end module pf_StringDescription
