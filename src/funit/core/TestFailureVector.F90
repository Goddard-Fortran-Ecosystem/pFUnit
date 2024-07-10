module PF_TestFailureVector
   use PF_TestFailure
!#define _type type (TestFailure)
!#define _vector TestFailureVector
!#define _iterator TestFailureVectorIterator
!#include "templates/vector.inc"

#define T TestFailure
#define Vector TestFailureVector
#define VectorIterator TestFailureVectorIterator
#include "vector/template.inc"

end module PF_TestFailureVector
