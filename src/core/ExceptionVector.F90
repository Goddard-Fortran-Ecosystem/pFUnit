module PF_ExceptionVector_mod
   use PF_Exception_mod
   
#define _type class(Exception)
#define _allocatable
#define _vector ExceptionVector
#define _iterator ExceptionVectorIterator
#include "templates/vector.inc"

end module PF_ExceptionVector_mod
