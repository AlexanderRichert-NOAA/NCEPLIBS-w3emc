! Unit tests for w3fi62() queue descriptor construction.
program test_w3fi62
  implicit none

  call test_total_size_nonzero_remainder_valid_catalog()
  call test_total_size_exact_80_blocks_invalid_catalog()
  call test_missing_increment_count_error()
  call test_total_size_too_small_error()

  print *, 'SUCCESS!'

contains

  subroutine test_total_size_nonzero_remainder_valid_catalog()
    character(len=80) :: loc
    integer :: kary(7), ierr

    loc = ' '
    kary = 0
    kary(1) = 9
    kary(2) = 16
    kary(3) = 1234
    kary(4) = 12345
    kary(7) = 161

    call w3fi62(loc, 'FTUS80', kary, ierr)

    call expect_equal_int(ierr, 0, 'nonzero remainder ierr')
    call expect_equal_int(kary(5), 3, 'nonzero remainder increments')
    call expect_equal_int(kary(6), 1, 'nonzero remainder last-byte count')
    call expect_equal_int(get_bits(loc, 160, 16), 3, 'descriptor increments field')
    call expect_equal_int(get_bits(loc, 176, 8), 1, 'descriptor last-byte field')
    call expect_equal_int(get_bits(loc, 304, 4), 1, 'creation time BCD hour tens')
    call expect_equal_int(get_bits(loc, 308, 4), 2, 'creation time BCD hour ones')
    call expect_equal_int(get_bits(loc, 312, 4), 3, 'creation time BCD minute tens')
    call expect_equal_int(get_bits(loc, 316, 4), 4, 'creation time BCD minute ones')
    call expect_equal_int(get_bits(loc, 360, 8), 0, 'zero-fill byte 46')
    call expect_equal_int(get_bits(loc, 632, 8), 0, 'zero-fill byte 80')
  end subroutine test_total_size_nonzero_remainder_valid_catalog

  subroutine test_total_size_exact_80_blocks_invalid_catalog()
    character(len=80) :: loc
    integer :: kary(7), ierr
    integer :: i, offset

    loc = ' '
    kary = 0
    kary(1) = 31
    kary(2) = 23
    kary(3) = 2359
    kary(4) = 0
    kary(7) = 160

    call w3fi62(loc, 'ABCD12', kary, ierr)

    call expect_equal_int(ierr, 0, 'exact-80-blocks ierr')
    call expect_equal_int(kary(5), 2, 'exact-80-blocks increments')
    call expect_equal_int(kary(6), 80, 'exact-80-blocks last-byte count')
    call expect_equal_int(get_bits(loc, 160, 16), 2, 'descriptor increments exact-80')
    call expect_equal_int(get_bits(loc, 176, 8), 80, 'descriptor last-byte exact-80')

    ! Invalid catalog input should force bytes 41-45 to '55555' in EBCDIC (0xF5).
    do i = 0, 4
      offset = 320 + 8 * i
      call expect_equal_int(get_bits(loc, offset, 8), 245, 'invalid catalog fallback byte')
    end do
  end subroutine test_total_size_exact_80_blocks_invalid_catalog

  subroutine test_missing_increment_count_error()
    character(len=80) :: loc
    integer :: kary(7), ierr

    loc = ' '
    kary = 0
    kary(1) = 10
    kary(2) = 11
    kary(3) = 1111
    kary(4) = 123
    kary(5) = 0
    kary(6) = 0
    kary(7) = 0

    call w3fi62(loc, 'EFGH34', kary, ierr)
    call expect_equal_int(ierr, 1, 'missing increment count error code')
  end subroutine test_missing_increment_count_error

  subroutine test_total_size_too_small_error()
    character(len=80) :: loc
    integer :: kary(7), ierr

    loc = ' '
    kary = 0
    kary(1) = 10
    kary(2) = 11
    kary(3) = 1111
    kary(4) = 123
    kary(7) = 34

    call w3fi62(loc, 'IJKL56', kary, ierr)
    call expect_equal_int(ierr, 2, 'too-small total-size error code')
  end subroutine test_total_size_too_small_error

  integer function get_bits(loc, iskip, nbits)
    character(len=80), intent(in) :: loc
    integer, intent(in) :: iskip, nbits
    integer :: tmp(1)
    integer :: loc_i(20)

    tmp(1) = 0
    loc_i = transfer(loc, loc_i)
    call gbyte(loc_i, tmp(1), iskip, nbits)
    get_bits = tmp(1)
  end function get_bits

  subroutine expect_equal_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_equal_int

end program test_w3fi62