#define T_TYPE integer
#define T_ALIAS int
#define T_HASH

#include "templates/vec.f90_template"


#define T_TYPE character
#define T_ALIAS char
#define T_VEC astring
#define T_HASH

#include "templates/vec.f90_template"


#define T_TYPE astring
#define T_ALIAS str
#define T_MOD astring_mod
#define T_HASH

#include "templates/vec.f90_template"


#define T_TYPE str_ref
#define T_ALIAS str_ref
#define T_MOD str_ref_mod
#define T_HASH

#include "templates/vec.f90_template"


#define T_TYPE vec_int
#define T_ALIAS vec_int
#define T_MOD vec_int_mod
#define T_HASH

#include "templates/vec.f90_template"
