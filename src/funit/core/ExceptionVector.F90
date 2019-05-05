module PF_ExceptionVector
   use PF_Exception
   
#define _type class(Exception)
#define _allocatable
#define _vector ExceptionVector
#define _iterator ExceptionVectorIterator
#include "templates/vector.inc"

end module PF_ExceptionVector
