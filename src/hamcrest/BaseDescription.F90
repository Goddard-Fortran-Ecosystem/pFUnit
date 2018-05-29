module pf_BaseDescription_mod
   use pf_MatcherDescription_mod
   implicit none
   private

   type, abstract, extends(MatcherDescription) :: BaseDescription
      private
   contains
      procedure :: append_text
      procedure :: append_description_of

      procedure :: append
   end type BaseDescription

contains


   subroutine append_text(this, text)
      class (BaseDescription), intent(inout) :: this
      character(*), intent(in) :: text

      call this%append(text)

   end subroutine append_text



   subroutine append(this, str)
      class (BaseDescription), intent(inout) :: this
      character(*), intent(in) :: str

      
   end subroutine append



end module pf_BaseDescription_mod
