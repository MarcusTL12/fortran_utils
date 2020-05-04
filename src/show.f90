module show_mod
    implicit none
    !
    private
    !
    public :: show
    interface show
        module procedure show_chars
        module procedure show_int
        module procedure show_char_p
    end interface
contains
    subroutine show_chars(s)
        implicit none
        !
        character(len=*), intent(in) :: s
        integer :: i
        !
        do i = 1, len(s)
            write (*, '(A)', advance='no') s(i:i)
        end do
    end subroutine
    !
    subroutine show_char_p(s)
        implicit none
        !
        character, intent(in) :: s(:)
        integer :: i
        !
        do i = 1, size(s)
            write (*, '(A)', advance='no') s(i)
        end do
    end subroutine
    !
    subroutine show_int(i)
        implicit none
        !
        integer, intent(in) :: i
        character(len=20)   :: buf
        integer :: j
        !
        write (buf, '(I20)') i
        !
        j = 1
        do while (buf(j:j) == ' ')
            j = j + 1
        end do
        call show(buf(j:20))
    end subroutine
end module
