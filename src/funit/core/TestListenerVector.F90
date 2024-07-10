module PF_TestListenerVector
   use PF_TestListener
#define T TestListener
#define T_polymorphic
#define Vector TestListenerVector
#define VectorIterator TestListenerVectorIterator
#include "vector/template.inc"

end module PF_TestListenerVector
