module pf_StringVector_mod
#ifdef __GFORTRAN__
  use pf_String_mod
#endif  
#define _vector StringVector
#define _iterator StringVectorIterator
#ifndef __GFORTRAN__
#  include "types/deferredLengthString.inc"
#else
#  define _type type(String)
#endif
#include "templates/vector.inc"
end module pf_StringVector_mod
