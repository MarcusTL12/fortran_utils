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
    implicit none
    !
    private
    !
    type json_obj
        integer(1) :: t
        type(c_ptr) :: data
    end type
end module