module pf_OptionVector_mod
   use pf_Option_mod
#define _vector OptionVector
#define _iterator OptionVectorIterator
#define _type type (Option)
#include "templates/vector.inc"
end module pf_OptionVector_mod
