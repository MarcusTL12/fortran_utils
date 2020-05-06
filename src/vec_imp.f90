#define T_TYPE integer
#define T_ALIAS int
#define T_HASH
#define T_SHOW

#include "templates/vec.f90_template"


#define T_TYPE astring
#define T_ALIAS str
#define T_MOD astring_mod
#define T_HASH
#define T_SHOW
#define T_SHOW_MOD astring_show_mod

#include "templates/vec.f90_template"


#define T_TYPE vec_int
#define T_ALIAS vec_int
#define T_MOD vec_int_mod
#define T_HASH
#define T_SHOW

#include "templates/vec.f90_template"


#define T_TYPE integer(1)
#define T_ALIAS int8
#define T_HASH
#define T_SHOW

#include "templates/vec.f90_template"


#define T_TYPE c_ptr
#define T_ALIAS cptr
#define T_MOD iso_c_binding
#define T_DISABLE_EQ

#include "templates/vec.f90_template"
