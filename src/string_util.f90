module string_util_mod
    use astring_mod
    use vec_str_mod
    !
    implicit none
    !
    interface show
        module procedure show_astring
        module procedure show_chars
        module procedure show_vec_str
        module procedure show_int
    end interface
    !
    interface tostring
        module procedure tostring_chars
    end interface
    !
    interface readline
        module procedure readline_to_string
    end interface
    !
    interface parse_int
        module procedure str_to_int
        module procedure chars_to_int
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
    function split_with_delim(str, delim) result(ret)
        implicit none
        !
        type(astring), intent(in) :: str
        character, intent(in)     :: delim
        type(vec_str)             :: ret
        type(astring)  :: buffer, buffer2
        integer        :: i
        !
        call ret%new()
        call buffer%new()
        !
        do i = 1, size(str)
            if (str%data(i) == delim) then
                buffer2 = buffer
                call ret%push(buffer2)
                call buffer%clear()
            else
                call buffer%push(str%data(i))
            end if
        end do
        !
        buffer2 = buffer
        call ret%push(buffer2)
        call buffer%clear()
    end function
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
    pure integer function str_to_int(s)
        implicit none
        !
        type(astring), intent(in) :: s
        character(len=20) :: buf
        integer :: i
        !
        buf = ''
        do i = 1, size(s)
            buf(i:i) = s%data(i)
        end do
        !
        str_to_int = parse_int(buf)
    end function
    !
    pure integer function chars_to_int(s)
        implicit none
        !
        character(len=*), intent(in) :: s
        !
        read (s, *) chars_to_int
    end function
end module
