module astring_mod
    implicit none
    !
    type astring
        character, allocatable :: data(:)
    contains
        procedure       :: new_astring_from_arr
        procedure       :: new_astring_from_string
        generic, public :: new => new_astring_from_arr, &
            new_astring_from_string
        generic, public :: assignment(=) => new_astring_from_arr, &
            new_astring_from_string
        final           :: astringfinalizer
        procedure       :: astring_eq
        generic, public :: operator(==) => astring_eq
        procedure       :: astring_neq
        generic, public :: operator(/=) => astring_neq
    end type
    !
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
contains
    subroutine astringfinalizer(self)
        implicit none
        !
        type(astring), intent(inout) :: self
        !
        print *, "trying to deallocate"
        !
        if (allocated(self%data)) then
            print *, "Deallocated: ", self%data
            deallocate (self%data)
        end if
    end subroutine astringfinalizer
    !
    subroutine new_astring_from_arr(self, str)
        implicit none
        !
        class(astring), intent(out) :: self
        character, intent(in) :: str(:)
        !
        self%data = str
    end subroutine
    !
    subroutine new_astring_from_string(self, str)
        implicit none
        !
        class(astring), intent(inout) :: self
        character(len=*), intent(in) :: str
        integer        :: i
        !
        call astringfinalizer(self)
        allocate (self%data(len_trim(str)))
        !
        do i = 1, size(self%data)
            self%data(i) = str(i:i)
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
end module
