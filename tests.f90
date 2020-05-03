program main
    use astring_mod
    use vec_int_mod
    use vec_str_mod
    use vec_vec_int_mod
    use string_util_mod
    use str_ref_mod
    use vec_str_ref_mod
    use map_int_int_mod
    use map_str_str_mod
    implicit none
    !
    character(len=10) :: arg
    !
    call getarg(1, arg)
    !
    select case (arg)
    case ('1')
        call test1()
    case ('2')
        call test2()
    case ('3')
        call test3()
    case ('4')
        call test4()
    case ('5')
        call test5()
    case ('6')
        call test6()
    case ('7')
        call test7()
    case ('8')
        call test8()
    case ('9')
        call test9()
    case ('10')
        call test10()
    case ('11')
        call test11()
    case default
        print *, "not implemented"
    end select
contains
    subroutine test1()
        implicit none
        !
        type(astring) :: a
        type(astring), target :: b
        type(astring), pointer :: c
        !
        a = tostring('Hello astring')
        call show(a)
        print *
        !
        call transfer(a, b)
        !
        b%data(2) = 'Q'
        !
        call show(b)
        print *
        !
        c => b
        !
        c%data(2) = 'H'
        !
        call show(b)
        print *
        call show(c)
        print *
        !
        a = c
        !
        call show(a)
        print *
        call show(b)
        print *
        call show(c)
        print *
    end subroutine
    !
    subroutine test2()
        implicit none
        !
        type(vec_int) :: v
        integer :: i, d(10)
        !
        d = (/3, 1, 4, 1, 5, 9, 2, 6, 5, 3/)
        !
        call v%with_capacity(1)
        !
        do i = 1, 10
            call v%push(d(i))
        end do
        !
        v%at(2) = 7
        !
        print *, v%data
        print *, size(v)
    end subroutine
    !
    subroutine test3()
        implicit none
        !
        type(vec_str) :: v
        type(astring) :: s
        type(astring), pointer :: a
        !
        call v%with_capacity(1)
        !
        s = tostring("Marcus")
        call v%push(s)
        !
        s = tostring("Takvam")
        call v%push(s)
        !
        s = tostring("Lexander")
        call v%push(s)
        !
        a => v%at(2)
        ! a%data(4) = 'V'
        s = a
        !
        call show(v)
    end subroutine
    !
    subroutine test4()
        implicit none
        !
        type(vec_vec_int) :: v
        type(vec_int) :: v1
        type(vec_int), pointer :: p
        integer :: i, j
        !
        call v%with_capacity(1)
        !
        do i = 1, 3
            call v1%with_capacity(1)
            do j = i, i + 3
                call v1%push(j)
            end do
            call v%push(v1)
        end do
        !
        do i = 1, 3
            p => v%at(i)
            do j = 1, 3
                print *, p%at(j)
            end do
            print *
        end do
    end subroutine
    !
    subroutine test5()
        implicit none
        !
        type(vec_str) :: a, b
        type(astring) :: s
        !
        call a%with_capacity(1)
        !
        s = tostring("Marcus")
        call a%push(s)
        !
        s = tostring("Takvam")
        call a%push(s)
        !
        s = tostring("Lexander")
        call a%push(s)
        !
        b = a
    end subroutine
    !
    subroutine test6()
        implicit none
        !
        type(vec_int) :: v
        !
        v = (/3, 1, 4, 1, 5, 9, 2/)
        !
        print *, v%data
    end subroutine
    !
    subroutine test7()
        implicit none
        !
        type(astring) :: s
        type(vec_str_ref) :: v
        integer :: i
        !
        s = tostring("Marcus,Takvam,Lexander")
        !
        call v%new()
        !
        call split_with_delim(s%as_slice(), ',', v)
        !
        do i = 1, size(v)
            call show(v%at(i))
            print *
        end do
    end subroutine
    !
    subroutine test8()
        implicit none
        !
        type(astring) :: buffer
        !
        call buffer%new()
        !
        open (1, file="res/testfile.txt")
        !
        do while (readline(1, buffer))
            call show(buffer)
            print *
            call buffer%clear()
        end do
        !
        close (1)
    end subroutine
    !
    subroutine test9()
        implicit none
        !
        type(astring) :: a
        character(len=5) :: b = "12345"
        !
        a = tostring("-31415")
        !
        call show(parse_int(a))
        print *
        call show(parse_int(b))
        print *
    end subroutine
    !
    subroutine test10()
        implicit none
        !
        type(map_int_int) :: m
        integer :: a(5), b(5), i
        logical :: stat
        integer, pointer :: k, v
        !
        call m%with_capacity(1)
        !
        print *, size(m%meta)
        !
        a = (/-1, 2, 3, 7, 100/)
        b = (/3, 1, 4, 1, 5/)
        !
        do i = 1, 5
            call m%insert(a(i), b(i))
        end do
        !
        print *, size(m%meta)
        !
        stat = .false.
        do while (m%next_kvp(k, v, stat))
            print *, k, " => ", v
        end do
    end subroutine
    !
    subroutine test11()
        implicit none
        !
        type(map_str_str) :: m
        type(astring) :: a, b
        type(astring), pointer :: k, v
        logical :: stat
        !
        call m%with_capacity(1)
        !
        a = tostring("Marcus")
        b = tostring("Lexander")
        call m%insert(a, b)
        !
        a = tostring("Ylva")
        b = tostring("Os")
        call m%insert(a, b)
        !
        a = tostring("Sverre")
        b = tostring("Olsen")
        call m%insert(a, b)
        !
        a = tostring("Bjørnar")
        b = tostring("Kaarevik")
        call m%insert(a, b)
        !
        stat = .false.
        do while (m%next_kvp(k, v, stat))
            call show(k)
            call show(" -> ")
            call show(v)
            print *
        end do
        !
        call show(m%get(tostring("Marcus")))
        print *
    end subroutine
end program main
