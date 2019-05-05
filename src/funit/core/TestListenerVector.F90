module PF_TestListenerVector
   use PF_TestListener
#define _type class (TestListener)
#define _allocatable
#define _vector TestListenerVector
#define _iterator TestListenerVectorIterator
#include "templates/vector.inc"

end module PF_TestListenerVector
