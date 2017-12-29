module PF_TestFailureVector_mod
   use PF_TestFailure_mod
#define _type type (TestFailure)
#define _vector TestFailureVector
#define _iterator TestFailureVectorIterator
#include "templates/vector.inc"

end module PF_TestFailureVector_mod
