module hash_mod
    implicit none
    !
    private
    !
    public     :: seed
    integer(8) :: seed = 3 * 3 * 13619 * 68743 * 424564793 - 4
    !
    public :: hash
    interface hash
        module procedure hash_int
        module procedure hash_int8
        module procedure hash_int_arr
        module procedure hash_char
        module procedure hash_str
    end interface
    !
    public :: fuse_hash
contains
    subroutine fuse_hash(a, b)
        implicit none
        !
        integer(8), intent(in) :: a
        integer(8), intent(inout) :: b
        !
        b = seed * ieor(a, b)
    end subroutine
    !
    function hash_int(a) result(h)
        implicit none
        !
        integer, intent(in) :: a
        integer(8)          :: h
        !
        h = seed
        call fuse_hash(int(a, 8), h)
    end function
    !
    function hash_int8(a) result(h)
        implicit none
        !
        integer(1), intent(in) :: a
        integer(8)          :: h
        !
        h = seed
        call fuse_hash(int(a, 8), h)
    end function
    !
    function hash_int_arr(a) result(h)
        implicit none
        !
        integer, intent(in) :: a(:)
        integer(8)          :: h
        integer :: i
        !
        h = seed
        !
        do i = 1, size(a)
            call fuse_hash(int(a(i), 8), h)
        end do
    end function
    !
    function hash_char(a) result(h)
        implicit none
        !
        character, intent(in) :: a
        integer(8)            :: h
        !
        h = seed
        call fuse_hash(int(ichar(a), 8), h)
    end function
    !
    function hash_str(a) result(h)
        implicit none
        !
        character, intent(in) :: a(:)
        integer(8)            :: h
        integer :: i
        !
        h = seed
        !
        do i = 1, size(a)
            call fuse_hash(hash(a(i)), h)
        end do
    end function
end module
