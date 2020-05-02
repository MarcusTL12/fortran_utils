module string_util_mod
    use astring_mod
    use vec_char_mod
    use vec_str_mod
    !
    implicit none
    !
    interface to_string
        module procedure vec_to_string
    end interface
contains
    subroutine vec_to_string(v, s)
        implicit none
        !
        type(vec_char), intent(inout) :: v
        type(astring), intent(inout)  :: s
        integer :: i
        !
        call s%new(size(v))
        !
        do i = 1, size(v)
            s%data(i) = v%data(i)
        end do
    end subroutine
    !
    function split_with_delim(str, delim) result(ret)
        implicit none
        !
        type(astring), intent(in) :: str
        character, intent(in)     :: delim
        type(vec_str)             :: ret
        type(vec_char) :: buffer
        type(astring)  :: sbuffer
        integer        :: i
        !
        call ret%new()
        call buffer%new()
        !
        do i = 1, size(str)
            if (str%data(i) == delim) then
                call to_string(buffer, sbuffer)
                call ret%push(sbuffer)
                call buffer%clear()
            else
                call buffer%push(str%data(i))
            end if
        end do
        !
        call to_string(buffer, sbuffer)
        call ret%push(sbuffer)
        call buffer%clear()
    end function
end module