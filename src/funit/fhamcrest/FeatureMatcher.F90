module pf_FeatureMatcher
  use pf_AbstractMatcher
  use pf_TypeSafeMatcher
  use pf_MatcherDescription
  implicit none
  private

  public :: FeatureMatcher

  type, abstract, extends(TypeSafeMatcher) :: FeatureMatcher
     private
     class(AbstractMatcher), allocatable :: submatcher
     character(:), allocatable :: feature_description
     character(:), allocatable :: feature_name
   contains
     procedure :: super ! setter/constructor
     procedure(feature_value_of), deferred :: feature_value_of
     procedure :: matches_safely
     procedure :: describe_mismatch_safely
     procedure :: describe_to
  end type FeatureMatcher

  abstract interface
     function feature_value_of(this, actual) result(feature_value)
       import FeatureMatcher
       class(*), allocatable :: feature_value
       class(FeatureMatcher), intent(in) :: this
       class(*), intent(in) :: actual
     end function feature_value_of
  end interface

contains

  subroutine super(this, submatcher, feature_description, feature_name)
    class(FeatureMatcher), intent(inout) :: this
    class(AbstractMatcher), intent(in) :: submatcher
    character(*), intent(in) :: feature_description
    character(*), intent(in) :: feature_name

    this%submatcher = submatcher
    this%feature_description = feature_description
    this%feature_name = feature_name
    
  end subroutine super


  logical function matches_safely(this, actual_value)
    class(FeatureMatcher), intent(in) :: this
    class(*), intent(in) :: actual_value

    matches_safely = this%submatcher%matches(this%feature_value_of(actual_value))
    
  end function matches_safely

  subroutine describe_mismatch_safely(this, actual, description)
    class(FeatureMatcher), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    call description%append_text(this%feature_name)
    call description%append_text(" ")
    call this%submatcher%describe_mismatch(this%feature_value_of(actual), description)

  end subroutine describe_mismatch_safely

  subroutine describe_to(this, description)
    class(FeatureMatcher), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text(this%feature_description)
    call description%append_text(" ")
    call description%append_description_of(this%submatcher)
    
  end subroutine describe_to



end module pf_FeatureMatcher
