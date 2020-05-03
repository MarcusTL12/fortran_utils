#define K_TYPE integer
#define K_ALIAS int

#define V_TYPE integer
#define V_ALIAS int

#include "templates/hashmap.f90_template"


#define K_TYPE astring
#define K_ALIAS str
#define K_MOD astring_mod

#define V_TYPE astring
#define V_ALIAS str
#define V_MOD astring_mod

#include "templates/hashmap.f90_template"
