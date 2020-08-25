module pf_RelationalMatcherV3
   use iso_fortran_env
   use pf_TypeSafeMatcher
   use pf_MatcherDescription
   implicit none
   private

   public RelationalMatcher

   type, abstract, extends(TypeSafeMatcher) :: RelationalMatcher
      private
      class(*), allocatable :: operand

      character(:), allocatable :: relation
      character(:), allocatable :: relation_description
      character(:), allocatable :: mismatch_msg
   contains
      procedure :: super
      procedure :: describe_to
      procedure :: describe_mismatch_safely

      procedure :: expects_type_of

      procedure(i_less), nopass, deferred :: less
      procedure :: matches_safely
   end type RelationalMatcher

   abstract interface
      recursive logical function i_less(actual, operand)
         import RelationalMatcher
         class(*), intent(in) :: actual
         class(*), intent(in) :: operand
      end function i_less
   end interface
contains
   subroutine super(this, relation, operand)
      class(RelationalMatcher), intent(inout) :: this
      character(*), intent(in) :: relation
      class(*), intent(in) :: operand

      this%operand = operand
      this%relation = relation

      select case(relation)
      case('<')
         this%relation_description = 'strictly less than '
         this%mismatch_msg = ' is greater than or equal to '
      case('<=')
         this%relation_description = 'less than or equal to '
         this%mismatch_msg = ' is strictly greater than '
      case('>')
         this%relation_description = 'strictly greater than '
         this%mismatch_msg = ' is less than or equal to '
      case('>=')
         this%relation_description = 'greater than or equal to '
         this%mismatch_msg = ' is strictly less than '
      case default
         error stop
      end select
   end subroutine super

   subroutine describe_to(this, description)
      class(RelationalMatcher), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_text("integer or real value ")
      call description%append_text(this%relation_description)
      call description%append_value(this%operand)
   end subroutine describe_to

   subroutine describe_mismatch_safely(this, actual, description)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      call description%append_value(actual)
      call description%append_text(this%mismatch_msg)
      call description%append_value(this%operand)
   end subroutine describe_mismatch_safely

   logical function expects_type_of(this, actual)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual

      select type(o => this%operand)
      type is (logical)
         select type(a => actual)
         type is (logical)
            expects_type_of = (kind(o) == kind(a))
         class default
            expects_type_of = .false.
         end select
      type is (integer)
         select type(a => actual)
         type is (integer)
            expects_type_of = (kind(o) == kind(a))
         class default
            expects_type_of = .false.
         end select
      type is (real)
         select type(a => actual)
         type is (real)
            expects_type_of = (kind(o) == kind(a))
         class default
            expects_type_of = .false.
         end select
      type is (complex)
         select type(a => actual)
         type is (complex)
            expects_type_of = (kind(o) == kind(a))
         class default
            expects_type_of = .false.
         end select
      type is (character(*))
         select type(a => actual)
         type is (character(*))
            expects_type_of = (kind(o) == kind(a))
         class default
            expects_type_of = .false.
         end select
      class default
         expects_type_of = same_type_as(this%operand, actual)
      end select
   end function expects_type_of

   recursive logical function matches_safely(this, actual_value)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual_value

      select case(this%relation)
      case('<')
         matches_safely = (this%less(actual_value, this%operand))
      case('>=')
         matches_safely = (.not. this%less(actual_value, this%operand))
      case('>')
         matches_safely = (this%less(this%operand, actual_value))
      case('<=')
         matches_safely = (.not. this%less(this%operand, actual_value))
      case default
         error stop
      end select
   end function matches_safely
end module pf_RelationalMatcherV3
