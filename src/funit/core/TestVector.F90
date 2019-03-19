module PF_TestVector
   use PF_Test
#define _type class (Test)
#define _allocatable
#define _vector TestVector
#define _iterator TestVectorIterator
#include "templates/vector.inc"

end module PF_TestVector
