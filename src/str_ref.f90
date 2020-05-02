module str_ref_mod
    implicit none
    !
    type str_ref
        character, pointer :: data(:)
    end type
    !
    interface transfer
        module procedure transfer_str_ref
    end interface
    !
    interface drop
        module procedure drop_str_ref
    end interface
    !
    interface show
        module procedure show_str_ref
    end interface
contains
    subroutine drop_str_ref(s)
        implicit none
        !
        type(str_ref), intent(inout) :: s
        return
        call size(s%data)
    end subroutine
    !
    subroutine transfer_str_ref(a, b)
        implicit none
        !
        type(str_ref), intent(inout) :: a, b
        !
        b = a
    end subroutine
    !
    subroutine show_str_ref(s)
        implicit none
        !
        type(str_ref), intent(in) :: s
        integer :: i
        !
        do i = 1, size(s%data)
            write (*, '(A)', advance='no') s%data(i)
        end do
    end subroutine
end module