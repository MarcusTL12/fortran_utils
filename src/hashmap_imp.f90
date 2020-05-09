#define K_TYPE integer
#define K_ALIAS int

#define V_TYPE integer
#define V_ALIAS int

#define M_SHOW

#include "templates/hashmap.f90_template"


#define K_TYPE astring
#define K_ALIAS str
#define K_MOD astring_mod
#define K_SHOW_MOD astring_show_mod

#define V_TYPE astring
#define V_ALIAS str
#define V_MOD astring_mod

#define M_SHOW

#include "templates/hashmap.f90_template"


#define K_TYPE astring
#define K_ALIAS str
#define K_MOD astring_mod

#define V_TYPE c_ptr
#define V_ALIAS cptr
#define V_MOD iso_c_binding

#include "templates/hashmap.f90_template"


#define K_TYPE astring
#define K_ALIAS str
#define K_MOD astring_mod

#define V_TYPE integer
#define V_ALIAS int

#include "templates/hashmap.f90_template"
