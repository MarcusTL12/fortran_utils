#define T_TYPE integer
#define T_ALIAS int

#include "templates/vec.f90_template"


#define T_TYPE character
#define T_ALIAS char

#include "templates/vec.f90_template"


#define T_TYPE astring
#define T_ALIAS str
#define T_MOD astring_mod

#include "templates/vec.f90_template"


#define T_TYPE vec_int
#define T_ALIAS vec_int
#define T_MOD vec_int_mod

#include "templates/vec.f90_template"
