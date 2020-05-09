! t enum:
! 1: null
! 2: bool/logical
! 3: integer
! 4: floating point/real
! 5: string
! 6: array/vec
! 7: map

module json2_mod
    use iso_c_binding
    use astring_mod
    use astring_show_mod
    use string_util_mod
    use map_str_int_mod
    use show_mod
    use vec_int8_mod
    use vec_int_mod
    implicit none
    !
    private
    !
    public :: parse_json_file, parse_json
    !
    public :: json_obj
    type json_obj
        integer(1) :: t
        integer    :: dp ! data pointer
    contains
        ! final :: json_finalize
    end type
    !
    public :: show
    interface show
        module procedure json_show
    end interface
    !
    public :: drop
    interface drop
        ! module procedure json_finalize
    end interface
contains
    function parse_json_file(filename, mem) result(json)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(vec_int8), intent(inout) :: mem
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
        json = parse_json(text%as_slice(), i, mem)
    end function
    ! !
    recursive function parse_json(text, i, mem) result(json)
        implicit none
        !
        character, intent(in) :: text(:)
        integer, intent(inout) :: i
        type(vec_int8), intent(inout) :: mem
        type(json_obj) :: json
        integer :: j
        !
        ! print *, i
        !
        do while (text(i) == ' ')
            i = i + 1
        end do
        !
        json%t = 0
        !
        if (str_eq(text(i:i + 3), str_p("null"))) then
            ! print *, "Found null"
            json%t = 1
            json%dp = 0
            i = i + 4
            return
        end if
        !
        if (str_eq(text(i:i + 3), str_p("true")) .or. &
            str_eq(text(i:i + 4), str_p("false"))) then
            ! print *, "Found bool"
            json = parse_json_bool(text(i:i + 4), i)
            return
        end if
        !
        if (is_numeric(text(i)) .or. text(i) == '-') then
            j = i
            do while (text(j + 1) /= ' ' .and. text(j + 1) /= ',' .and. &
                      text(j + 1) /= ']' .and. text(j + 1) /= '}')
                j = j + 1
            end do
            !
            if (is_numeric(text(i:j))) then
                ! print *, "Found int"
                json = parse_json_int(text(i:j))
            else
                ! print *, "Found real"
                json = parse_json_real(text(i:j))
            end if
            i = j + 1
            return
        end if
        !
        if (text(i) == '"') then
            ! print *, "Found string"
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
            ! print *, "Found array"
            i = i + 1
            json = parse_json_array(text, i)
            return
        end if
        !
        if (text(i) == '{') then
            ! print *, "Found map"
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
            logical, target :: b
            integer(1), pointer :: bytes(:)
            !
            call c_f_pointer(c_loc(b), bytes, [sizeof(b)])
            !
            json%t = 2
            json%dp = size(mem) + 1
            !
            b = .not. str_eq(text, str_p("false"))
            call mem%extend(bytes)
            !
            if (b) then
                i = i + 4
            else
                i = i + 5
            end if
        end function
        !
        function parse_json_int(text) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            type(json_obj) :: json
            integer, target :: n
            integer(1), pointer :: bytes(:)
            !
            json%t = 3
            json%dp = size(mem) + 1
            !
            call c_f_pointer(c_loc(n), bytes, [sizeof(n)])
            n = parse_int(text)
            !
            call mem%extend(bytes)
        end function
        !
        function parse_json_real(text) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            type(json_obj) :: json
            real, target :: n
            integer(1), pointer :: bytes(:)
            !
            json%t = 3
            json%dp = size(mem) + 1
            !
            call c_f_pointer(c_loc(n), bytes, [sizeof(n)])
            n = parse_real(text)
            !
            call mem%extend(bytes)
        end function
        !
        function parse_json_string(text) result(json)
            implicit none
            !
            character, target, intent(in) :: text(:)
            type(json_obj) :: json
            type(astring), target :: s
            integer(1), pointer :: bytes(:)
            !
            json%t = 5
            json%dp = size(mem) + 1
            !
            call c_f_pointer(c_loc(s), bytes, [sizeof(s)])
            s = text
            !
            call mem%extend(bytes)
        end function
        !
        function parse_json_array(text, i) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            integer, intent(inout) :: i
            type(json_obj) :: json
            type(json_obj), target :: buff
            type(vec_int), target :: v
            logical :: done
            integer(1), pointer :: bytes(:)
            integer :: ibuf
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
                    buff = parse_json(text, i, mem)
                    !
                    ibuf = size(mem) + 1
                    call v%push(ibuf)
                    !
                    call c_f_pointer(c_loc(buff), bytes, [sizeof(buff)])
                    call mem%extend(bytes)
                end if
            end do
            !
            json%t = 6
            json%dp = size(mem) + 1
            !
            call c_f_pointer(c_loc(v), bytes, [sizeof(v)])
            !
            call mem%extend(bytes)
        end function
        !
        function parse_json_map(text, i) result(json)
            implicit none
            !
            character, intent(in) :: text(:)
            integer, intent(inout) :: i
            type(json_obj) :: json
            type(json_obj), target :: buff
            type(astring) :: k_buff
            type(map_str_int), target :: m
            integer(1), pointer :: bytes(:)
            logical :: done
            integer :: j, ibuf
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
                    buff = parse_json(text, i, mem)
                    ibuf = size(mem) + 1
                    call m%insert(k_buff, ibuf)
                    !
                    call c_f_pointer(c_loc(buff), bytes, [sizeof(buff)])
                    call mem%extend(bytes)
                end if
            end do
            !
            json%t = 7
            json%dp = size(mem) + 1
            !
            call c_f_pointer(c_loc(m), bytes, [sizeof(m)])
            call mem%extend(bytes)
        end function
    end function
    !
    subroutine json_show(json, mem)
        implicit none
        !
        type(json_obj), intent(in)  :: json
        type(vec_int8), intent(in)  :: mem
        logical, pointer            :: bp
        integer, pointer            :: ip
        real, pointer               :: rp
        type(astring), pointer      :: sp
        type(vec_int), pointer      :: vp
        type(map_str_int), pointer  :: mp
        type(json_obj), pointer     :: jp
        integer :: i
        logical :: first, stat
        !
        select case (json%t)
        case (1)
            call show("null")
        case (2)
            call c_f_pointer(c_loc(mem%at(json%dp)), bp)
            call show(bp)
        case (3)
            call c_f_pointer(c_loc(mem%at(json%dp)), ip)
            call show(ip)
        case (4)
            call c_f_pointer(c_loc(mem%at(json%dp)), rp)
            call show(rp)
        case (5)
            call c_f_pointer(c_loc(mem%at(json%dp)), sp)
            call show(sp)
        case (6)
            call c_f_pointer(c_loc(mem%at(json%dp)), vp)
            call show('[')
            first = .true.
            do i = 1, size(vp)
                if (.not. first) call show(", ")
                first = .false.
                call c_f_pointer(c_loc(mem%at(vp%at(i))), jp)
                call show(jp, mem)
            end do
            call show(']')
        case (7)
            call c_f_pointer(c_loc(mem%at(json%dp)), mp)
            call show('{')
            first = .true.
            stat = .false.
            do while (mp%next_kvp(sp, ip, stat))
                if (.not. first) call show(", ")
                first = .false.
                call c_f_pointer(c_loc(mem%at(ip)), jp)
                call show(sp)
                call show(" => ")
                call show(jp, mem)
            end do
            call show('}')
        end select
    end subroutine
    !
    ! subroutine json_finalize(json)
    !     implicit none
    !     !
    !     type(json_obj) :: json
    !     logical, pointer            :: bp
    !     integer, pointer            :: ip
    !     real, pointer               :: rp
    !     type(astring), pointer      :: sp
    !     type(vec_cptr), pointer     :: vp
    !     type(map_str_cptr), pointer :: mp
    !     type(json_obj), pointer     :: jp
    !     type(c_ptr), pointer        :: cp
    !     integer :: i
    !     logical :: stat
    !     !
    !     ! print *, "dropping", json%t
    !     !
    !     select case (json%t)
    !     case (2)
    !         call c_f_pointer(json%data, bp)
    !         deallocate (bp)
    !     case (3)
    !         call c_f_pointer(json%data, ip)
    !         deallocate (ip)
    !     case (4)
    !         call c_f_pointer(json%data, rp)
    !         deallocate (rp)
    !     case (5)
    !         call c_f_pointer(json%data, sp)
    !         call drop(sp)
    !         deallocate (sp)
    !     case (6)
    !         call c_f_pointer(json%data, vp)
    !         do i = 1, size(vp)
    !             call c_f_pointer(vp%at(i), jp)
    !             deallocate (jp)
    !         end do
    !         deallocate (vp)
    !     case (7)
    !         call c_f_pointer(json%data, mp)
    !         stat = .false.
    !         do while (mp%next_kvp(sp, cp, stat))
    !             call c_f_pointer(cp, jp)
    !             deallocate (jp)
    !         end do
    !         deallocate (mp)
    !     end select
    ! end subroutine
end module
