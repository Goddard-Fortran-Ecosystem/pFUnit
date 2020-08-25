module fHamcrest
  use pf_MatcherDescription
  use pf_StringDescription
  use pf_MatcherAssert

  use pf_AbstractMatcher
  use pf_Is
  use pf_IsNot
!  use pf_RelationalMatcherV2
  use pf_IsEqual
  use pf_Int32RelationalMatcher
  use pf_Int64RelationalMatcher
  use pf_Real32RelationalMatcher
  use pf_Real64RelationalMatcher
  use pf_Real128RelationalMatcher
!  use pf_IsGreater
!  use pf_IsGreaterOrEqual
!  use pf_IsLess
!  use pf_IsLessOrEqual
  use pf_IsTrueOrFalse
  use pf_IsNear
  use pf_IsRelativelyNear
  use pf_AnyOf
  use pf_AllOf
  use pf_Every
  use pf_StringContains
  use pf_StringStartsWith
  use pf_StringEndsWith
  use pf_IsArrayWithSize

  use pf_Matchable
end module fHamcrest
