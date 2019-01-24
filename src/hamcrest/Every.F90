module pf_Every
  use iso_fortran_env
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_MatcherDescription
  use pf_Array
  implicit none
  private

  public :: Every
  public :: every_item

  type, extends(BaseMatcher) :: Every
    private
    class(AbstractMatcher), allocatable :: item_matcher
   contains
     procedure :: matches
     procedure :: describe_mismatch
     procedure :: describe_to
  end type Every


  interface every_item
     module procedure every_item_
  end interface every_item


contains


  function every_item_(item_matcher) result(matcher)
    type(Every) :: matcher
    class(AbstractMatcher), intent(in) :: item_matcher
    matcher%item_matcher = item_matcher
  end function every_item_


  logical function matches(this, actual_value)
    class(Every), intent(in) :: this
    class(*), intent(in) :: actual_value

    integer :: i

    select type (a => actual_value)
    class is (internal_array_1d)
       do i = 1, size(a%items)
          if (.not. this%item_matcher%matches(a%items(i))) then
             matches = .false.
             return
          end if
       end do
       matches = .true.
    class default
       matches = .false. ! wrong type/rank
    end select

  end function matches

  subroutine describe_mismatch(this, actual, description)
    class(Every), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    integer :: i

    select type (actual)
    type is (internal_array_1d)
       do i = 1, size(actual%items)
          if (.not. this%item_matcher%matches(actual%items(i))) then
             call this%item_matcher%describe_mismatch(actual%items(i), description)
             return
          end if
       end do
    class default
       call description%append_text("was not a 1-D array")
    end select
  end subroutine describe_mismatch

  subroutine describe_to(this, description)
    class(Every), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("every item is ")
    call description%append_description_of(this%item_matcher)

  end subroutine describe_to

end module pf_Every
