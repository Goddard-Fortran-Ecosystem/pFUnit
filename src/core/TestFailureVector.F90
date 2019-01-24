module PF_TestFailureVector
   use PF_TestFailure
#define _type type (TestFailure)
#define _vector TestFailureVector
#define _iterator TestFailureVectorIterator
#include "templates/vector.inc"

end module PF_TestFailureVector
