module PF_ExceptionVector
   use PF_Exception
   
#define T Exception
#define T_polymorphic
#define Vector ExceptionVector
#define VectorIterator ExceptionVectorIterator
#include "vector/template.inc"

end module PF_ExceptionVector
