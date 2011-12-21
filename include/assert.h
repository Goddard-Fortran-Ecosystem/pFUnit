!# macros for dynamic (program-by-contract) assertions
!# failures halt execution.

#define ASSERT_ALWAYS(cond) Call Assert_(cond,__FILE__,__LINE__)

#ifdef NDEBUG
#define ASSERT
#else
#define ASSERT(cond) ASSERT_ALWAYS(cond)
#endif
