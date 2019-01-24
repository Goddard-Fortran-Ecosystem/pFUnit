module pf_SelfDescribingVector
  use pf_SelfDescribing

#define _type class(SelfDescribing)
#define _vector SelfDescribingVector
#define _vectorIterator SelfDescribingVectorIterator
#define _allocatable

#include "templates/vector.inc"

#undef _allocatable
#undef _vectorIterator
#undef _vector
#undef _type
  
end module pf_SelfDescribingVector
