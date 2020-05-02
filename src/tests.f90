program main
    use astring_mod
    use vec_int_mod
    use vec_str_mod
    use vec_vec_int_mod
    use string_util_mod
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
        a = 'Hello astring'
        !
        call transfer(a, b)
        !
        b%data(2) = 'Q'
        !
        print *, a
        print *, b
        !
        c => b
        !
        c%data(2) = 'H'
        !
        print *, b
        print *, c
        !
        a = c
        !
        print *, a
        print *, b
        print *, c
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
        s = "Marcus"
        call v%push(s)
        !
        s = "Takvam"
        call v%push(s)
        !
        s = "Lexander"
        call v%push(s)
        !
        a => v%at(2)
        ! a%data(4) = 'V'
        s = a
        !
        print *, v%data
        print *, s
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
        s = "Marcus"
        call a%push(s)
        !
        s = "Takvam"
        call a%push(s)
        !
        s = "Lexander"
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
        type(vec_str) :: v
        integer :: i
        !
        s = "Marcus,Takvam,Lexander"
        !
        v = split_with_delim(s, ',')
        !
        do i = 1, size(v)
            call show(v%at(i))
            print *
        end do
    end subroutine
end program main
