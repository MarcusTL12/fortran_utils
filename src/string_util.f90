module string_util_mod
    use astring_mod
    use vec_str_mod
    !
    implicit none
    !
    private
    !
    public :: split_with_delim
    !
    public :: show
    interface show
        module procedure show_astring
        module procedure show_chars
        module procedure show_vec_str
        module procedure show_int
    end interface
    !
    public :: tostring
    interface tostring
        module procedure tostring_chars
    end interface
    !
    public :: readline
    interface readline
        module procedure readline_to_string
    end interface
    !
    public :: parse_int
    interface parse_int
        module procedure str_slice_to_int
        module procedure str_to_int
        module procedure chars_to_int
    end interface
    !
    public :: append_str
    interface append_str
        module procedure append_str_str
        module procedure append_str_chars
    end interface
contains
    subroutine show_astring(s)
        implicit none
        !
        type(astring), intent(in) :: s
        integer :: i
        !
        do i = 1, size(s)
            write (*, '(A)', advance='no') s%data(i)
        end do
    end subroutine
    !
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
    subroutine show_vec_str(v)
        implicit none
        !
        type(vec_str), intent(in) :: v
        integer :: i
        !
        call show('['//new_line('A'))
        !
        do i = 1, size(v)
            call show('    "')
            call show(v%data(i))
            call show('"'//new_line('A'))
        end do
        !
        call show(']')
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
    !
    function tostring_chars(c) result(s)
        implicit none
        !
        character(len=*), intent(in) :: c
        type(astring) :: s
        character :: buff
        integer :: i
        !
        call s%with_capacity(len(c))
        !
        do i = 1, len(c)
            buff = c(i:i)
            call s%push(buff)
        end do
    end function
    !
    subroutine split_with_delim(str, delim, ret)
        implicit none
        !
        character, target, intent(in)   :: str(:)
        character, intent(in)           :: delim
        type(vec_str), intent(inout)    :: ret
        character, pointer :: p(:)
        type(astring)  :: buffer
        integer        :: i, j
        !
        j = 1
        do i = 1, size(str)
            if (str(i) == delim) then
                p => str(j:i - 1)
                call buffer%from_borrow(p)
                call ret%push(buffer)
                j = i + 1
            end if
        end do
        !
        p => str(j:size(str))
        call buffer%from_borrow(p)
        call ret%push(buffer)
    end subroutine
    !
    logical function readline_to_string(unit, s) result(res)
        implicit none
        !
        integer, intent(in) :: unit
        type(astring), intent(inout) :: s
        integer :: ios, i
        !
        character(len=256) :: buffer
        character :: cbuf
        !
        res = .true.
        !
        do
            read (unit, '(A)', advance='no', err=7, end=8, eor=7, iostat=ios) &
                buffer
            !
            do i = 1, 256
                cbuf = buffer(i:i)
                call s%push(cbuf)
            end do
        end do
        !
7       do i = 1, len_trim(buffer)
            cbuf = buffer(i:i)
            call s%push(cbuf)
        end do
        return
8       res = .false.
    end function
    !
    integer function str_to_int(s)
        implicit none
        !
        type(astring), intent(in) :: s
        !
        str_to_int = parse_int(s%as_slice())
    end function
    !
    integer function str_slice_to_int(s)
        implicit none
        !
        character, intent(in) :: s(:)
        character(len=20) :: buf
        integer :: i
        !
        buf = ''
        do i = 1, size(s)
            buf(i:i) = s(i)
        end do
        !
        str_slice_to_int = parse_int(buf)
    end function
    !
    integer function chars_to_int(s)
        implicit none
        !
        character(len=*), intent(in) :: s
        !
        read (s, *) chars_to_int
    end function
    !
    subroutine append_str_str(a, b)
        implicit none
        !
        type(astring), intent(inout) :: a
        character(len=*), intent(in) :: b
        character :: buff
        integer :: i
        !
        do i = 1, len_trim(b)
            buff = b(i:i)
            call a%push(buff)
        end do
    end subroutine
    !
    subroutine append_str_chars(a, b)
        implicit none
        !
        type(astring), intent(inout) :: a
        character, intent(in) :: b(:)
        character :: buff
        integer :: i
        !
        do i = 1, size(b)
            buff = b(i)
            call a%push(buff)
        end do
    end subroutine
end module
