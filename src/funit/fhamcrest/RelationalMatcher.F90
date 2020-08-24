module pf_RelationalMatcher
   use iso_fortran_env
   use pf_TypeSafeMatcher
   use pf_MatcherDescription
   implicit none
   private

   public :: RelationalMatcher

   type, abstract, extends(TypeSafeMatcher) :: RelationalMatcher
      ! private
      character(:), allocatable :: operation
      character(:), allocatable :: relation_description
      character(:), allocatable :: mismatch_msg
      character(:), allocatable :: operand_decription
   contains
      procedure(i_less), nopass, deferred :: less
      procedure :: describe_to
      procedure :: describe_mismatch_safely
      procedure :: matches_safely
   end type RelationalMatcher

   

contains


   subroutine super(this, operation, operand_decription)
      class(RelationalMatcher), intent(inout) :: this
      character(*), intent(in) :: operation
      
      this%operation = operation
      this%operand_decription = operand_decription

      select case (operation)
      case ('<')
         this%relation_description = 'strictly less than'
         this%mismatch_msg = ' is greater than or equal to '
      case ('<=')
         this%relation_description = 'less than'
         this%mismatch_msg = ' is strictly greater than '
      case ('>')
         this%relation_description = 'strictly greater than'
         this%mismatch_msg = ' is less than or equal to '
      case ('>=')
         this%relation_description = 'greater than'
         this%mismatch_msg = ' is strictly less than '
      case default
         error stop
      end select

   end subroutine super


   subroutine describe_to(this, description)
      class(RelationalMatcher), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_text(this%relation_description)
      call description%append_value(this%operand_description)

   end subroutine describe_to

   subroutine describe_mismatch_safely(this, actual, description)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      call description%append_value(actual)
      call description%append_text(this%mismatch_msg)
      call description%append_value(this%expected_value)

   end subroutine describe_mismatch_safely

   recursive logical function matches_safely(this, actual)
      class(Int32RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual

      select case (this%operation)
      case ('<')
         matches_safely = this%less(this%get_operand, actual)
      case ('<=')
         matches_safely = .not. this%less(actual, this%get_operand)
      case ('>')
         matches_safely = this%less(actual, this%get_operand)
      case ('>=')
         matches_safely = .not. this%less(this%get_operand, actual)
      end select
      
   end function matches_safely
   
end module pf_RelationalMatcher
