#include "unused_dummy.fh"

module pf_AllOf
  use pf_AbstractMatcher
  use pf_MatcherDescription
  use pf_BaseMatcher
  use pf_MatcherVector
  use pf_SelfDescribingVector
  implicit none
  private

  public :: AllOf
  public :: all_of

  type, extends(BaseMatcher) :: AllOf
     type(MatcherVector) :: matchers
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_to_op
     procedure :: describe_mismatch
  end type AllOf


  interface all_of
     module procedure all_of_vector
     module procedure all_of_array
     module procedure all_of_multi
  end interface all_of


contains


  function all_of_vector(matchers) 
    type (AllOf) :: all_of_vector
    type (MatcherVector), intent(in) :: matchers

    all_of_vector%matchers = matchers

  end function all_of_vector

  function all_of_array(matchers)
    type (AllOf) :: all_of_array
    class(AbstractMatcher), intent(in) :: matchers(:)

    integer :: i

    do i = 1, size(matchers)
       call all_of_array%matchers%push_back(matchers(i))
    end do
    
  end function all_of_array

  
  ! Poor person's equivalent of Java varargs
  function all_of_multi(matcher_1, matcher_2, matcher_3, matcher_4)
    type (AllOf) :: all_of_multi
    class(AbstractMatcher), optional, intent(in) :: matcher_1
    class(AbstractMatcher), optional, intent(in) :: matcher_2
    class(AbstractMatcher), optional, intent(in) :: matcher_3
    class(AbstractMatcher), optional, intent(in) :: matcher_4

    if (present(matcher_1)) then
       call all_of_multi%matchers%push_back(matcher_1)
    end if

    if (present(matcher_2)) then
       call all_of_multi%matchers%push_back(matcher_2)
    end if

    if (present(matcher_3)) then
       call all_of_multi%matchers%push_back(matcher_3)
    end if

    if (present(matcher_4)) then
       call all_of_multi%matchers%push_back(matcher_4)
    end if

  end function all_of_multi


  logical function matches(this, actual_value)
    class(AllOf), intent(in) :: this
    class(*), intent(in) :: actual_value

    type(MatcherVectorIterator) :: iter
    class(AbstractMatcher), pointer :: matcher

    iter = this%matchers%begin()
    do while (iter /= this%matchers%end())
       matcher => iter%get()
       if (.not. matcher%matches(actual_value)) then
          matches = .false.
          return
       end if
       call iter%next()
    end do

    matches = .true.

  end function matches

  subroutine describe_to(this, description)
    class(AllOf), intent(in):: this
    class(MatcherDescription), intent(inout) :: description

    call this%describe_to_op(description, "and")

  end subroutine describe_to

  subroutine describe_to_op(this, description, operator)
    class(AllOf), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description
    character(*), intent(in) :: operator

    type (SelfDescribingVector), allocatable :: matchers_as_SelfDescribing
    type (MatcherVectorIterator) :: iter
    
    allocate(matchers_as_SelfDescribing) ! ensure empty at each iteration
    iter = this%matchers%begin()
    do while (iter /= this%matchers%end())
       call matchers_as_SelfDescribing%push_back(iter%get())
       call iter%next()
    end do

    call description%append_list("(", " " // operator // " ", ")", matchers_as_SelfDescribing)
  end subroutine describe_to_op
  

  subroutine describe_mismatch(this, actual, description)
    class(AllOf), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    _UNUSED_DUMMY(this)

    call description%append_text("was ")
    call description%append_value(actual)

  end subroutine describe_mismatch


end module pf_AllOf
