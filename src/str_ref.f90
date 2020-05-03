module str_ref_mod
    use hash_mod
    !
    implicit none
    !
    type str_ref
        character, pointer :: data(:)
    contains
        procedure :: eq_str_ref
        generic, public :: operator(==) => eq_str_ref
        procedure :: neq_str_ref
        generic, public :: operator(/=) => neq_str_ref
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
    !
    interface hash
        module procedure hash_str_ref
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
    !
    function hash_str_ref(s) result(h)
        implicit none
        !
        type(str_ref), intent(in) :: s
        integer(8) :: h
        !
        h = hash(s%data)
    end function
    !
    logical function eq_str_ref(self, other)
        implicit none
        !
        class(str_ref), intent(in) :: self
        type(str_ref), intent(in) :: other
        integer :: i
        !
        if (size(self%data) /= size(other%data)) then
            eq_str_ref = .false.
            return
        end if
        !
        eq_str_ref = .true.
        do i = 1, size(self%data)
            if (self%data(i) /= other%data(i)) then
                eq_str_ref = .false.
                exit
            end if
        end do
    end function
    !
    logical function neq_str_ref(self, other)
        implicit none
        !
        class(str_ref), intent(in) :: self
        type(str_ref), intent(in) :: other
        integer :: i
        !
        if (size(self%data) /= size(other%data)) then
            neq_str_ref = .false.
            return
        end if
        !
        neq_str_ref = .true.
        do i = 1, size(self%data)
            if (self%data(i) /= other%data(i)) then
                neq_str_ref = .false.
                exit
            end if
        end do
    end function
end module
