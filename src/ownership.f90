module ownership_mod
    implicit none
    !
    private
    !
    public :: transfer
    interface transfer
        module procedure transfer_int
        module procedure transfer_int8
        module procedure transfer_char
    end interface
    !
    public :: drop
    interface drop
        module procedure drop_int
        module procedure drop_char
    end interface
contains
    subroutine transfer_int(a, b)
        implicit none
        !
        integer, intent(inout) :: a, b
        !
        b = a
    end subroutine
    !
    subroutine transfer_int8(a, b)
        implicit none
        !
        integer(1), intent(inout) :: a, b
        !
        b = a
    end subroutine
    !
    subroutine transfer_char(a, b)
        implicit none
        !
        character, intent(inout) :: a, b
        !
        b = a
    end subroutine
    !
    subroutine drop_int(a)
        implicit none
        !
        integer, intent(inout) :: a
        a = a
    end subroutine
    !
    subroutine drop_char(a)
        implicit none
        !
        character, intent(inout) :: a
        a = a
    end subroutine
end module
