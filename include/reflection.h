
#ifdef STRINGIFY_QUOTE
#define TOSTRING(name) '"'name'"'
#endif

#ifdef STRINGIFY_SIMPLE
#define TOSTRING(name) "name"
#endif

#ifdef STRINGIFY_OPERATOR
#define TOSTRING(name) #name
#endif


#define REFLECT(name) TOSTRING(name), name
