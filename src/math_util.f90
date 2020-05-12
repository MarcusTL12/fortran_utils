module math_util_mod
    use vec_int_mod
    implicit none
    !
    private
    !
    public :: next_permutation, clamp, prime, factorize, next_factor, &
              sum_divisors, nextprime, isprime
    !
    integer, parameter :: amt_primes = 1230
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
    !
    logical function isprime(n)
        implicit none
        !
        integer, intent(in) :: n
        integer :: i
        !
        isprime = .true.
        do i = 1, amt_primes
            if (n == prime(i)) return
            if (modulo(n, prime(i)) == 0) then
                isprime = .false.
                return
            end if
        end do
        !
        do i = prime(amt_primes) + 2, ceiling(sqrt(real(n))), 2
            if (modulo(n, i) == 0) then
                isprime = .false.
                return
            end if
        end do
    end function
    !
    integer function nextprime(n) result(m)
        implicit none
        !
        integer, intent(in) :: n
        !
        m = n
        if (isprime(m)) return
        if (modulo(m, 2) == 0) m = m + 1
        !
        do while (.not. isprime(m))
            m = m + 2
        end do
    end function
    !
    subroutine next_factor(n, i, p, e)
        implicit none
        !
        integer, intent(inout) :: n
        integer, intent(inout) :: p, e
        integer, intent(inout) :: i
        integer :: pr
        !
        if (i <= amt_primes) then
            do while (prime(i) <= n)
                if (modulo(n, prime(i)) == 0) then
                    p = prime(i)
                    e = 1
                    n = n / p
                    !
                    do while (modulo(n, p) == 0)
                        e = e + 1
                        n = n / p
                    end do
                    return
                end if
                !
                i = i + 1
                if (i > amt_primes) exit
            end do
        end if
        !
        pr = nextprime(p + 1)
        do while (pr <= n)
            if (modulo(n, pr) == 0) then
                p = pr
                e = 1
                n = n / p
                !
                do while (modulo(n, p) == 0)
                    e = e + 1
                    n = n / p
                end do
                return
            end if
            !
            pr = nextprime(pr + 2)
        end do
    end subroutine
    !
    subroutine factorize(n, p, e)
        implicit none
        !
        integer, intent(in) :: n
        type(vec_int), intent(inout) :: p, e
        integer :: i, m, pr, ex
        !
        call p%clear()
        call e%clear()
        !
        i = 1
        m = n
        !
        do while (m > 1)
            call next_factor(m, i, pr, ex)
            call p%push(pr)
            call e%push(ex)
        end do
    end subroutine
    !
    integer function sum_divisors(n)
        implicit none
        !
        integer, intent(in) :: n
        integer :: m, i, j, p, e, s, r
        !
        m = n
        i = 1
        p = 1
        sum_divisors = 1
        do while (m > 1)
            call next_factor(m, i, p, e)
            s = 0
            r = 1
            do j = 0, e
                s = s + r
                r = r * p
            end do
            sum_divisors = sum_divisors * s
        end do
    contains
        integer function ipow(b, e)
            implicit none
            !
            integer, intent(in) :: b, e
            integer :: i
            !
            ipow = 1
            do i = 1, e
                ipow = ipow * b
            end do
        end function
    end function
end module
