module pf_SelfDescribingVector
  use pf_SelfDescribing

#define T SelfDescribing
#define T_polymorphic
#define Vector SelfDescribingVector
#define VectorIterator SelfDescribingVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module pf_SelfDescribingVector
