module pf_TypeSafeSelfDescribing
  use pf_SurrogateDescription
  use pf_SelfDescribing
  use pf_MatcherDescription
  implicit none
  private

  public :: TypeSafeSelfDescribing

  type, abstract, extends(SelfDescribing) :: TypeSafeSelfDescribing
   contains
     procedure(describe_to), deferred :: describe_to
     procedure :: type_unsafe_describe_to
  end type TypeSafeSelfDescribing

   abstract interface

      subroutine describe_to(this, description)
         import TypeSafeSelfDescribing
         import MatcherDescription
         class (TypeSafeSelfDescribing), intent(in) :: this
         class (MatcherDescription), intent(inout) :: description
      end subroutine describe_to

   end interface

 contains


   recursive subroutine type_unsafe_describe_to(this, description)
     class (TypeSafeSelfDescribing), intent(in) :: this
     class (SurrogateDescription), intent(inout) :: description

     select type (description)
     class is (MatcherDescription)
        call this%describe_to(description)
     class default
        ERROR STOP "Must use subclass of MatcherDescription, not SurrogateDescription."
     end select

   end subroutine type_unsafe_describe_to

 end module pf_TypeSafeSelfDescribing
    
