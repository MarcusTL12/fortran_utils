program main
    use astring_mod
    use vec_mod_int
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
    case default
        print *, "not implemented"
    end select
contains
    subroutine test1()
        implicit none
        !
        type(astring) :: test
        !
        test = 'Hello astring'
        !
        print *, test
        !
        test = 'Bye astring'
        !
        print *, size(test)
    end subroutine
    !
    subroutine test2()
        implicit none
        !
        type(vec_int) :: v
        !
        v%data = (/1, 2, 3, 4/)
        !
        print *, v%data
    end subroutine
end program main
