module pf_ArgVector_mod
   use pf_Arg_mod
#define _vector ArgVector
#define _iterator ArgVectorIterator
#define _type type (Arg)
#include "templates/vector.inc"
end module pf_ArgVector_mod
