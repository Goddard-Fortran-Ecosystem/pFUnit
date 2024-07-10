module pf_MatcherVector
  use pf_AbstractMatcher

#define T AbstractMatcher
#define T_polymorphic
#define Vector MatcherVector
#define VectorIterator MatcherVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T


end module pf_MatcherVector
