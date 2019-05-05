#include "unused_dummy.fh"

module pf_AnyOf
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_MatcherDescription
  use pf_MatcherVector
  use pf_SelfDescribingVector
  implicit none
  private

  public :: AnyOf
  public :: any_of

  type, extends(BaseMatcher) :: AnyOf
     private
     type(MatcherVector) :: matchers
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_to_op
     procedure :: describe_mismatch
  end type AnyOf


  interface any_of
     module procedure any_of_array
     module procedure any_of_vector
     module procedure any_of_multi
     
  end interface any_Of


contains


  function any_of_array(matchers)
    type(AnyOf) :: any_of_array
    class(AbstractMatcher), intent(in) :: matchers(:)

    integer :: i

    do i = 1, size(matchers)
       call any_of_array%matchers%push_back(matchers(i))
    end do

  end function any_of_array


  function any_of_vector(matchers) 
    type(AnyOf) :: any_of_vector
    type(MatcherVector), intent(in) :: matchers

    any_of_vector%matchers = matchers

  end function any_of_vector


  ! Poor person's equivalent of Java varargs
  function any_of_multi(matcher_1, matcher_2, matcher_3, matcher_4)
    type (AnyOf) :: any_of_multi
    class(AbstractMatcher), optional, intent(in) :: matcher_1
    class(AbstractMatcher), optional, intent(in) :: matcher_2
    class(AbstractMatcher), optional, intent(in) :: matcher_3
    class(AbstractMatcher), optional, intent(in) :: matcher_4

    if (present(matcher_1)) then
       call any_of_multi%matchers%push_back(matcher_1)
    end if

    if (present(matcher_2)) then
       call any_of_multi%matchers%push_back(matcher_2)
    end if

    if (present(matcher_3)) then
       call any_of_multi%matchers%push_back(matcher_3)
    end if

    if (present(matcher_4)) then
       call any_of_multi%matchers%push_back(matcher_4)
    end if

  end function any_of_multi


  logical function matches(this, actual_value)
    class(AnyOf), intent(in) :: this
    class(*), intent(in) :: actual_value

    
    type(MatcherVectorIterator) :: iter
    class(AbstractMatcher), pointer :: matcher

    iter = this%matchers%begin()
    do while (iter /= this%matchers%end())
       matcher => iter%get()
       if (matcher%matches(actual_value)) then
          matches = .true.
          return
       end if
       call iter%next()
    end do

    matches = .false.

  end function matches


  subroutine describe_to(this, description)
    class(AnyOf), intent(in):: this
    class(MatcherDescription), intent(inout) :: description

    call this%describe_to_op(description, "or")

  end subroutine describe_to

  subroutine describe_to_op(this, description, operator)
    class(AnyOf), intent(in) :: this
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
    class(AnyOf), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    _UNUSED_DUMMY(this)
    
    call description%append_text("was ")
    call description%append_value(actual)

  end subroutine describe_mismatch


end module pf_AnyOf
