module pf_MatcherVector
  use pf_AbstractMatcher

#define _type class(AbstractMatcher)
#define _vector MatcherVector
#define _vectorIterator MatcherVectorIterator
#define _allocatable

#include "templates/vector.inc"

#undef _allocatable
#undef _vectorIterator
#undef _vector
#undef _type
  
end module pf_MatcherVector
