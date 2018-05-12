module PF_TestVector_mod
   use PF_Test_mod
#define _type class (Test)
#define _allocatable
#define _vector TestVector
#define _iterator TestVectorIterator
#include "templates/vector.inc"

end module PF_TestVector_mod
