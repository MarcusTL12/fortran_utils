module md5_mod
    implicit none
    !
    private
    !
    integer, parameter :: shifts(64) = (/ &
                   7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, &
                       5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, &
                   4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, &
                    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21 &
                          /)
    integer, parameter :: k(64) = (/ &
                   -680876936, -389564586, 606105819, -1044525330, -176418897, &
                  1200080426, -1473231341, -45705983, 1770035416, -1958414417, &
                      -42063, -1990404162, 1804603682, -40341101, -1502002290, &
                   1236535329, -165796510, -1069501632, 643717713, -373897302, &
                      -701558691, 38016083, -660478335, -405537848, 568446438, &
                  -1019803690, -187363961, 1163531501, -1444681467, -51403784, &
                    1735328473, -1926607734, -378558, -2022574463, 1839030562, &
                  -35309556, -1530992060, 1272893353, -155497632, -1094730640, &
                      681279174, -358537222, -722521979, 76029189, -640364487, &
                    -421815835, 530742520, -995338651, -198630844, 1126891415, &
                    -1416354905, -57434055, 1700485571, -1894986606, -1051523, &
                  -2054922799, 1873313359, -30611744, -1560198380, 1309151649, &
                          -145523070, -1120210379, 718787259, -343485551 &
                          /)
    !
    character, parameter :: hex_chars(16) = (/ &
                            '0', '1', '2', '3', '4', '5', '6', '7', &
                            '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' &
                            /)
    !
    public :: md5
    interface md5
        module procedure md5_bytes
        module procedure md5_chars
    end interface
    !
    public :: md5_to_hex
contains
    subroutine md5_inner_loop(h, m)
        implicit none
        !
        integer, intent(inout) :: h(4)
        integer, intent(in)    :: m(16)
        integer :: a, b, c, d, f, i, j
        !
        a = h(1)
        b = h(2)
        c = h(3)
        d = h(4)
        !
        do i = 0, 63
            select case (i)
            case (0:15)
                f = ior(iand(b, c), iand(not(b), d))
                j = i
            case (16:31)
                f = ior(iand(d, b), iand(not(d), c))
                j = iand(5 * i + 1, 15)
            case (32:47)
                f = ieor(ieor(b, c), d)
                j = iand(3 * i + 5, 15)
            case (48:63)
                f = ieor(c, ior(b, not(d)))
                j = iand(7 * i, 15)
            end select
            !
            f = f + a + k(i + 1) + m(j + 1)
            a = d
            d = c
            c = b
            b = b + ishftc(f, shifts(i + 1))
        end do
        !
        h = h + (/a, b, c, d/)
    end subroutine
    !
    function md5_bytes(m) result(h)
        use iso_c_binding
        implicit none
        !
        integer(1), target, intent(in) :: m(:)
        integer(1), target :: h(16), m_blk(64)
        integer, pointer :: m_blkp(:), h_p(:)
        integer :: rest, i
        !
        call c_f_pointer(c_loc(h), h_p, [4])
        !
        h_p(1) = 1732584193
        h_p(2) = -271733879
        h_p(3) = -1732584194
        h_p(4) = 271733878
        !
        rest = modulo(size(m), 64)
        !
        m_blk = 0
        m_blk(1:rest) = m(size(m) - rest + 1:size(m))
        m_blk(rest + 1) = -128
        m_blk(57:64) = transfer(int(size(m), 8) * 8, m_blk(57:64))
        !
        i = 1
        do while (i * 64 < size(m))
            call c_f_pointer(c_loc(m((i - 1) * 64 + 1:i * 64)), m_blkp, [16])
            call md5_inner_loop(h_p, m_blkp)
            i = i + 1
        end do
        !
        call c_f_pointer(c_loc(m_blk), m_blkp, [16])
        call md5_inner_loop(h_p, m_blkp)
    end function
    !
    function md5_chars(m) result(h)
        use iso_c_binding
        implicit none
        !
        character, target, intent(in) :: m(:)
        integer(1) :: h(16)
        integer(1), pointer :: m_p(:)
        !
        call c_f_pointer(c_loc(m), m_p, [size(m)])
        !
        h = md5_bytes(m_p)
    end function
    !
    pure function byte_to_hex(b) result(h)
        implicit none
        !
        integer(1), intent(in) :: b
        character :: h(2)
        !
        h(1) = hex_chars(ishft(b, -4) + 1)
        h(2) = hex_chars(iand(b, 15) + 1)
    end function
    !
    function md5_to_hex(h) result(hex)
        implicit none
        !
        integer(1), intent(in) :: h(16)
        character :: hex(32)
        integer   :: i
        !
        do i = 1, 16
            hex(2 * i - 1:2 * i) = byte_to_hex(h(i))
        end do
    end function
end module
