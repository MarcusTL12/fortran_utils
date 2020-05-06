! t enum:
! 1: null
! 2: bool/logical
! 3: integer
! 4: floating point/real
! 5: string
! 6: array/vec
! 7: map

module json_mod
    use iso_c_binding
    use astring_mod
    use astring_show_mod
    use string_util_mod
    use vec_cptr_mod
    use map_str_cptr_mod
    use show_mod
    implicit none
    !
    private
    !
    public :: parse_json_file, parse_json
    !
    public :: json_obj
    type json_obj
        integer(1) :: t
        type(c_ptr) :: data
    end type
    !
    public :: show
    interface show
        module procedure json_show
    end interface
contains
    function parse_json_file(filename) result(json)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(json_obj) :: json
        type(astring) :: text
        integer :: i
        !
        call text%new()
        open (1, file=filename)
        do while (readline(1, text))
        end do
        close (1)
        !
        i = 1
        json = parse_json(text%as_slice(), i)
    end function
    !
    recursive function parse_json(text, i) result(json)
        implicit none
        !
        character, intent(in) :: text(:)
        integer, intent(inout) :: i
        type(json_obj) :: json
        integer :: j
        !
        do while (text(i) == ' ')
            i = i + 1
        end do
        !
        json%t = 0
        !
        if (str_eq(text(i:i + 3), str_p("null"))) then
            json%t = 1
            json%data = c_null_ptr
            i = i + 4
            return
        end if
        !
        if (str_eq(text(i:i + 3), str_p("true")) .or. &
            str_eq(text(i:i + 4), str_p("false"))) then
            json = parse_json_bool(text(i:i + 4), i)
            return
        end if
        !
        if (is_numeric(text(i))) then
            j = i
            do while (text(j + 1) /= ' ' .and. text(j + 1) /= ',' .and. &
                      text(j + 1) /= ']' .and. text(j + 1) /= '}')
                j = j + 1
            end do
            !
            if (is_numeric(text(i:j))) then
                json = parse_json_int(text(i:j))
            else
                json = parse_json_real(text(i:j))
            end if
            i = j + 1
            return
        end if
        !
        if (text(i) == '"') then
            j = i
            do while (text(j + 1) /= '"')
                j = j + 1
            end do
            json = parse_json_string(text(i + 1:j))
            i = j + 2
            return
        end if
        !
        if (text(i) == '[') then
            i = i + 1
            json = parse_json_array(text, i)
            return
        end if
        !
        if (text(i) == '{') then
            i = i + 1
            json = parse_json_map(text, i)
            return
        end if
        !
        print *, "Something went horribly wrong"
    contains
        function parse_json_bool(text, i) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            integer, intent(inout) :: i
            type(json_obj) :: json
            logical, pointer :: b
            !
            allocate (b)
            !
            b = .not. str_eq(text, str_p("false"))
            !
            if (b) then
                i = i + 4
            else
                i = i + 5
            end if
            !
            json%t = 2
            json%data = c_loc(b)
        end function
        !
        function parse_json_int(text) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            type(json_obj) :: json
            integer, pointer :: n
            !
            allocate (n)
            n = parse_int(text)
            !
            json%t = 3
            json%data = c_loc(n)
        end function
        !
        function parse_json_real(text) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            type(json_obj) :: json
            real, pointer :: n
            !
            allocate (n)
            n = parse_real(text)
            !
            json%t = 4
            json%data = c_loc(n)
        end function
        !
        function parse_json_string(text) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            type(json_obj) :: json
            type(astring), pointer :: s
            !
            allocate (s)
            s = text
            !
            json%t = 5
            json%data = c_loc(s)
        end function
        !
        function parse_json_array(text, i) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            integer, intent(inout) :: i
            type(json_obj) :: json
            type(json_obj), pointer :: buff
            type(c_ptr) :: p_buff
            type(vec_cptr), pointer :: v
            logical :: done
            !
            allocate (v)
            !
            call v%new()
            !
            done = .false.
            !
            do while (.not. done)
                !
                do while (text(i) == ' ' .or. text(i) == ',')
                    i = i + 1
                end do
                !
                if (text(i) == ']') then
                    i = i + 1
                    done = .true.
                else
                    allocate (buff)
                    buff = parse_json(text, i)
                    !
                    p_buff = c_loc(buff)
                    !
                    call v%push(p_buff)
                end if
            end do
            !
            json%t = 6
            json%data = c_loc(v)
        end function
        !
        function parse_json_map(text, i) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            integer, intent(inout) :: i
            type(json_obj) :: json
            type(json_obj), pointer :: buff
            type(c_ptr) :: p_buff
            type(astring) :: k_buff
            type(map_str_cptr), pointer :: m
            logical :: done
            integer :: j
            !
            allocate (m)
            !
            call m%new()
            !
            done = .false.
            !
            do while (.not. done)
                !
                do while (text(i) == ' ' .or. text(i) == ',')
                    i = i + 1
                end do
                !
                if (text(i) == '}') then
                    i = i + 1
                    done = .true.
                else
                    j = i + 1
                    do while (text(j + 1) /= '"')
                        j = j + 1
                    end do
                    !
                    k_buff = text(i + 1:j)
                    !
                    i = j + 2
                    !
                    do while (text(i) /= ':')
                        i = i + 1
                    end do
                    !
                    i = i + 1
                    !
                    allocate (buff)
                    !
                    buff = parse_json(text, i)
                    p_buff = c_loc(buff)
                    call m%insert(k_buff, p_buff)
                end if
            end do
            !
            json%t = 7
            json%data = c_loc(m)
        end function
    end function
    !
    subroutine json_show(json)
        implicit none
        !
        type(json_obj) :: json
        logical, pointer            :: bp
        integer, pointer            :: ip
        real, pointer               :: rp
        type(astring), pointer      :: sp
        type(vec_cptr), pointer     :: vp
        type(map_str_cptr), pointer :: mp
        type(json_obj), pointer     :: jp
        type(c_ptr), pointer        :: cp
        integer :: i
        logical :: first, stat
        !
        select case (json%t)
        case (1)
            call show("null")
        case (2)
            call c_f_pointer(json%data, bp)
            call show(bp)
        case (3)
            call c_f_pointer(json%data, ip)
            call show(ip)
        case (4)
            call c_f_pointer(json%data, rp)
            call show(rp)
        case (5)
            call c_f_pointer(json%data, sp)
            call show(sp)
        case (6)
            call c_f_pointer(json%data, vp)
            call show('[')
            first = .true.
            do i = 1, size(vp)
                if (.not. first) call show(", ")
                first = .false.
                call c_f_pointer(vp%at(i), jp)
                call show(jp)
            end do
            call show(']')
        case (7)
            call c_f_pointer(json%data, mp)
            call show('{')
            first = .true.
            stat = .false.
            do while (mp%next_kvp(sp, cp, stat))
                if (.not. first) call show(", ")
                first = .false.
                call c_f_pointer(cp, jp)
                call show(sp)
                call show(" => ")
                call show(jp)
            end do
            call show('}')
        end select
    end subroutine
end module
