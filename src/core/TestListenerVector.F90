module PF_TestListenerVector_mod
   use PF_TestListener_mod
#define _type class (TestListener)
#define _allocatable
#define _vector TestListenerVector
#define _iterator TestListenerVectorIterator
#include "templates/vector.inc"

end module PF_TestListenerVector_mod
