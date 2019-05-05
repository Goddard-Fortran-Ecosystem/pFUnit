#include "unused_dummy.fh"

module pf_IsArrayWithSize
  use pf_AbstractMatcher
  use pf_MatcherDescription
  use pf_FeatureMatcher
  use pf_DescribedAs
  use pf_IsEqual
  use pf_Array
  implicit none
  private

  public :: IsArrayWithSize
  public :: array_with_size
  public :: empty_array

  type, extends(FeatureMatcher) :: IsArrayWithSize
     private
   contains
     procedure :: feature_value_of
     procedure :: expects_type_of
  end type IsArrayWithSize
  

  interface IsArrayWithSize
     module procedure new_IsArrayWithSize
  end interface IsArrayWithSize

  interface array_with_size
     module procedure array_with_size_size
     module procedure array_with_size_matcher
  end interface array_with_size


contains

  function new_IsArrayWithSize(size_matcher) result(matcher)
    type(IsArrayWithSize) :: matcher
    class(AbstractMatcher), intent(in) :: size_matcher

    call matcher%super(size_matcher, "an array with size", "array size")
  end function new_IsArrayWithSize

  function array_with_size_size(size) result(matcher)
    type(IsArrayWithSize) :: matcher
    integer, intent(in) :: size

    matcher = array_with_size(equal_to(size))
  end function array_with_size_size

  function array_with_size_matcher(size_matcher) result(matcher)
    type(IsArrayWithSize) :: matcher
    class(AbstractMatcher) :: size_matcher
    matcher = IsArrayWithSize(size_matcher)
  end function array_with_size_matcher


  function feature_value_of(this, actual) result(feature_value)
    class(*), allocatable :: feature_value
    class(IsArrayWithSize), intent(in) :: this
    class(*), intent(in) :: actual

    _UNUSED_DUMMY(this)
    select type (actual)
    type is (internal_array_1d)
       feature_value = size(actual%items)
    class default
       ERROR STOP __FILE__ // " This case is guarded againts by TypeSafeMatcher."
    end select

  end function feature_value_of


  function empty_array()
    class(DescribedAs), allocatable :: empty_array

    empty_array = described_as("an empty array", array_with_size(0))

  end function empty_array


  logical function expects_type_of(this, actual)
    class(IsArrayWithSize), intent(in) :: this
    class(*), intent(in) :: actual

    _UNUSED_DUMMY(this)

    select type(actual)
    type is (internal_array_1d)
       expects_type_of = .true.
    class default
       expects_type_of = .false.
    end select
       
  end function expects_type_of
  
end module pf_IsArrayWithSize
