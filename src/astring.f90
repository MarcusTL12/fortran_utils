#define T_TYPE character
#define T_ALIAS char
#define T_VEC astring
#define T_HASH

#include "templates/vec.f90_template"

module astring_show_mod
    use astring_mod
    use show_mod
    implicit none
    !
    private
    public :: show
    interface show
        module procedure show_astring
    end interface
contains
    subroutine show_astring(s)
        implicit none
        !
        type(astring), intent(in) :: s
        !
        call show('"')
        call show(s%as_slice())
        call show('"')
    end subroutine
end module
