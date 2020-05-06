module hash_mod
    use md5_mod
    use iso_c_binding
    implicit none
    !
    private
    !
    ! public     :: seed
    integer(8), parameter :: seed = 3 * 3 * 13619 * 68743 * 424564793 - 4
    !
    integer, parameter :: ord1(8) = (/2, 5, 6, 3, 7, 1, 8, 4/), &
                          ord2(8) = (/4, 7, 1, 6, 8, 5, 3, 2/)
    !
    public :: hash
    interface hash
        module procedure hash_int
        module procedure hash_int8
        module procedure hash_char
    end interface
    !
    public :: fuse_hash
contains
    subroutine fuse_hash(a, b)
        implicit none
        !
        integer(8), target, intent(in) :: a
        integer(8), target, intent(inout) :: b
        integer(1), pointer :: p(:), q(:)
        integer(1) :: buff(8)
        integer :: i
        !
        call c_f_pointer(c_loc(a), p, [8])
        call c_f_pointer(c_loc(b), q, [8])
        !
        do i = 1, 8
            buff(i) = ieor(p(ord1(i)), q(ord2(i)))
        end do
        !
        b = transfer(buff, b)
    end subroutine
    !
    function md5_trunc(x)
        implicit none
        !
        integer(1), intent(in) :: x(:)
        integer(8) :: md5_trunc
        ! integer(1) :: h(16)
        integer :: i
        !
        ! h = md5(x)
        ! md5_trunc = transfer(h(1:8), md5_trunc)
        !
        md5_trunc = seed
        do i = 1, size(x)
            md5_trunc = md5_trunc * seed + x(i)
        end do
    end function
    !
    function hash_int(a) result(h)
        implicit none
        !
        integer, intent(in), target :: a
        integer(8), target  :: h
        integer(1), pointer :: p(:)
        !
        call c_f_pointer(c_loc(a), p, [4])
        h = md5_trunc(p)
    end function
    !
    function hash_int8(a) result(h)
        implicit none
        !
        integer(1), intent(in) :: a
        integer(8)          :: h
        !
        h = md5_trunc((/a/))
    end function
    !
    ! function hash_int_arr(a) result(h)
    !     implicit none
    !     !
    !     integer, intent(in) :: a(:)
    !     integer(8)          :: h
    !     integer :: i
    !     !
    !     h = seed
    !     !
    !     do i = 1, size(a)
    !         call fuse_hash(int(a(i), 8), h)
    !     end do
    ! end function
    !
    function hash_char(a) result(h)
        implicit none
        !
        character, intent(in) :: a
        integer(8)            :: h
        !
        h = md5_trunc((/int(ichar(a), 1)/))
    end function
    !
    ! function hash_str(a) result(h)
    !     implicit none
    !     !
    !     character, intent(in) :: a(:)
    !     integer(8)            :: h
    !     integer :: i
    !     !
    !     h = seed
    !     !
    !     do i = 1, size(a)
    !         call fuse_hash(hash(a(i)), h)
    !     end do
    ! end function
end module
