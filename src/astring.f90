module astring_mod
    use ownership_mod
    !
    implicit none
    !
    private
    !
    public :: astring
    type astring
        character, pointer :: data(:)
    contains
        procedure       :: new => new_astring
        procedure       :: astring_from_arr
        procedure       :: astring_from_string
        procedure       :: astring_from_astring
        generic, public :: assignment(=) => astring_from_arr, &
            astring_from_string, astring_from_astring
        final           :: astringfinalizer
        procedure       :: astring_eq
        generic, public :: operator(==) => astring_eq
        procedure       :: astring_neq
        generic, public :: operator(/=) => astring_neq
    end type
    !
    public :: size
    interface size
        module procedure astring_size
    end interface
    !
    public :: write (unformatted)
    interface write (unformatted)
        module procedure astring_write_u
    end interface
    !
    public :: write (formatted)
    interface write (formatted)
        module procedure astring_write_f
    end interface
    !
    public :: transfer
    interface transfer
        module procedure astring_transfer
    end interface
    !
    public :: drop
    interface drop
        module procedure astringfinalizer
    end interface
    !
    public :: show
    interface show
        module procedure astring_show
    end interface
contains
    subroutine new_astring(self, l)
        implicit none
        !
        class(astring), intent(out) :: self
        integer, intent(in) :: l
        !
        call drop(self)
        !
        allocate (self%data(l))
    end subroutine
    !
    subroutine astringfinalizer(self)
        implicit none
        !
        type(astring), intent(inout) :: self
        !
        ! print *, "trying to deallocate"
        !
        if (associated(self%data)) then
            print *, "Deallocated: ", self%data
            deallocate (self%data)
        end if
    end subroutine astringfinalizer
    !
    subroutine astring_from_arr(self, str)
        implicit none
        !
        class(astring), intent(out) :: self
        character, intent(in)       :: str(:)
        integer :: i
        !
        ! call astringfinalizer(self)
        ! !
        ! allocate (self%data(size(str)))
        !
        call self%new(size(str))
        !
        do i = 1, size(str)
            self%data(i) = str(i)
        end do
    end subroutine
    !
    subroutine astring_from_string(self, str)
        implicit none
        !
        class(astring), intent(inout) :: self
        character(len=*), intent(in) :: str
        integer        :: i
        !
        ! call astringfinalizer(self)
        ! allocate (self%data(len_trim(str)))
        !
        call self%new(len_trim(str))
        !
        do i = 1, size(self%data)
            self%data(i) = str(i:i)
        end do
    end subroutine
    !
    subroutine astring_from_astring(self, other)
        implicit none
        !
        class(astring), intent(inout)   :: self
        type(astring), intent(in)       :: other
        integer :: i
        !
        ! call astringfinalizer(self)
        ! !
        ! allocate (self%data(size(other)))
        !
        call self%new(size(other))
        !
        do i = 1, size(other)
            self%data(i) = other%data(i)
        end do
    end subroutine
    !
    pure logical function astring_eq(self, rhs) result(eq)
        implicit none
        !
        class(astring), intent(in) :: self
        type(astring), intent(in) :: rhs
        integer :: i
        !
        eq = .true.
        if (size(self) /= size(rhs)) then
            eq = .false.
        else
            do i = 1, size(self)
                eq = self%data(i) == rhs%data(i)
                if (.not. eq) return
            end do
        end if
    end function
    !
    pure logical function astring_neq(self, rhs) result(eq)
        implicit none
        !
        class(astring), intent(in) :: self
        type(astring), intent(in)  :: rhs
        !
        eq = .not. (self == rhs)
    end function
    !
    pure integer function astring_size(self) result(s)
        implicit none
        !
        type(astring), intent(in) :: self
        !
        s = size(self%data)
    end function
    !
    subroutine astring_write_u(self, unit, iostat, iomsg)
        implicit none
        !
        class(astring), intent(in)    :: self
        integer, intent(in)    :: unit
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        !
        write (unit, iostat=iostat, iomsg=iomsg) self%data
        print *, self%data
    end subroutine
    !
    subroutine astring_write_f(self, unit, iotype, v_list, iostat, iomsg)
        implicit none
        !
        class(astring), intent(in)    :: self
        integer, intent(in)    :: unit
        character(len=*), intent(in)    :: iotype
        integer, intent(in)    :: v_list(:)
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        integer :: dummy1
        !
        dummy1 = size(v_list)
        dummy1 = len(iotype)
        !
        write (unit, *, iostat=iostat, iomsg=iomsg) self%data
    end subroutine
    !
    subroutine astring_transfer(a, b)
        implicit none
        !
        type(astring), intent(inout) :: a, b
        !
        b%data => a%data
        nullify (a%data)
    end subroutine
    !
    subroutine astring_show(str)
        implicit none
        !
        type(astring), intent(in) :: str
        integer :: i
        !
        write (*, '(A)', advance='no') '"'
        do i = 1, size(str)
            write (*, '(A)', advance='no') str%data(i)
        end do
        write (*, '(A)', advance='no') '"'
    end subroutine
end module
