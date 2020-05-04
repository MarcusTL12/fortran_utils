module math_util_mod
    implicit none
    !
    private
    !
    public :: inc
    interface inc
        module procedure inc_int
    end interface
contains
    pure subroutine inc_int(a, b)
        implicit none
        !
        integer, intent(inout) :: a
        integer, intent(in)    :: b
        !
        a = a + b
    end subroutine
end module
