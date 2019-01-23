module pf_MatcherVector_mod
  use pf_AbstractMatcher_mod

#define _type class(AbstractMatcher)
#define _vector MatcherVector
#define _vectorIterator MatcherVectorIterator
#define _allocatable

#include "templates/vector.inc"

#undef _allocatable
#undef _vectorIterator
#undef _vector
#undef _type
  
end module pf_MatcherVector_mod
