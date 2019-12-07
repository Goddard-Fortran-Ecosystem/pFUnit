module pf_Every
  use iso_fortran_env
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_MatcherDescription
  use pf_AbstractArrayWrapper
  use pf_ArrayWrapper

  implicit none
  private

  public :: Every
  public :: every_item

  type, extends(BaseMatcher) :: Every
    private
    class(AbstractMatcher), allocatable :: item_matcher
   contains
     procedure :: matches
     procedure :: matches_list
     procedure :: describe_mismatch
     procedure :: describe_first_mismatch
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

    select type (a => actual_value)
    class is (AbstractArrayWrapper)
       matches = this%matches_list(a%get()) ! reduce rank
    class default ! scalar
       matches = this%item_matcher%matches(actual_value)
    end select

  end function matches

  logical function matches_list(this, actual_values)
    class(Every), intent(in) :: this
    class(*), intent(in) :: actual_values(:)

    integer :: i

    do i = 1, size(actual_values)
       if (.not. this%matches(actual_values(i))) then
          matches_list = .false.
          return
       end if
    end do

    matches_list = .true.

  end function matches_list

  subroutine describe_mismatch(this, actual, description)
    class(Every), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    integer, allocatable :: index(:)
    class(*), allocatable :: items(:)

    print*,__FILE__,__LINE__
    select type (a => actual)
    class is (AbstractArrayWrapper)
       print*,__FILE__,__LINE__
       call a%to_list(items)
!!$       items = a%get()
       print*,__FILE__,__LINE__
       allocate(index(0))
       print*,__FILE__,__LINE__
       call this%describe_first_mismatch(items, description, index)
       print*,__FILE__,__LINE__
       call description%append_text(' at index ')
       print*,__FILE__,__LINE__
       call description%append_value('[',',',']',index)
    print*,__FILE__,__LINE__
    class default ! scalar
    print*,__FILE__,__LINE__
       call this%item_matcher%describe_mismatch(a, description)
    print*,__FILE__,__LINE__
    end select

  end subroutine describe_mismatch

  recursive subroutine describe_first_mismatch(this, actuals, description, index)
    class(Every), intent(in) :: this
    class(*), intent(in) :: actuals(:)
    class(MatcherDescription), intent(inout) :: description
    integer, allocatable, intent(inout) :: index(:)

    integer :: i

    class(*), allocatable :: item    
    do i = 1, size(actuals)
       item = actuals(i)
       if (.not. this%matches(item)) then
          select type (item)
          class is (AbstractArrayWrapper)
             call this%describe_first_mismatch(item%get(), description, index)
             index = [index, i]
          class default ! scalar
             call this%item_matcher%describe_mismatch(actuals(i), description)
             index = [i]
          end select
          return
       end if
    end do

  end subroutine describe_first_mismatch

  subroutine describe_to(this, description)
    class(Every), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("every item ")
    call description%append_description_of(this%item_matcher)

  end subroutine describe_to

end module pf_Every
