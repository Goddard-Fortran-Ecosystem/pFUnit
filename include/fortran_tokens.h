#ifdef HAS_CONCATENATION_OPERATOR
#define CONCATENATE(a,b) a ## b
#else
#define IDENTITY(x) x
#define CONCATENATE(a,b) IDENTITY(a)IDENTITY(b)
#endif

