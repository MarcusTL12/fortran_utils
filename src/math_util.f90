module math_util_mod
    use vec_int_mod
    implicit none
    !
    private
    !
    public :: next_permutation, clamp, prime
    !
    integer, allocatable :: primes(:)
    !
    public :: inc
    interface inc
        module procedure inc_int
    end interface
contains
    pure subroutine inc_int(a, b)
        implicit none
        !
        integer, intent(inout) :: a
        integer, intent(in)    :: b
        !
        a = a + b
    end subroutine
    !
    logical function next_permutation(p, s, u) result(nlast)
        implicit none
        !
        integer, intent(inout) :: p(:), s(:)
        logical, intent(inout) :: u(:)
        integer :: i
        !
        call fill_permutation()
        !
        i = 1
        nlast = .true.
        !
        do while (i <= size(s))
            s(i) = s(i) + 1
            if (s(i) > size(p) - i) then
                s(i) = 0
                if (i == size(s)) nlast = .false.
            else
                exit
            end if
            i = i + 1
        end do
    contains
        subroutine fill_permutation()
            implicit none
            !
            integer :: i, j, k
            !
            u = .false.
            do i = 1, size(s)
                j = 1
                k = 0
                do while (k < (s(i)) .or. u(j))
                    if (.not. u(j)) k = k + 1
                    j = j + 1
                end do
                p(i) = j
                u(j) = .true.
            end do
            do i = 1, size(u)
                if (.not. u(i)) p(size(p)) = i
            end do
        end subroutine
    end function
    !
    pure integer function clamp(n, l, h)
        implicit none
        !
        integer, intent(in) :: n, l, h
        !
        clamp = min(max(n, l), h)
    end function
    !
    subroutine load_primes()
        implicit none
        !
        integer, parameter :: amt_primes = 1229
        integer :: p, i
        !
        allocate (primes(amt_primes))
        open (1, file="res/primes.txt")
        !
        do i = 1, amt_primes
            read (1, *) p
            primes(i) = p
        end do
        !
        close (1)
    end subroutine
    !
    integer function prime(i)
        implicit none
        !
        integer, intent(in) :: i
        !
        if (.not. allocated(primes)) call load_primes()
        !
        prime = primes(i)
    end function
end module
