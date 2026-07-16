! Unit test for makwmo() WMO header assembly.
program test_makwmo
  implicit none

  character(len=1) :: header(21)

  call makwmo('FTUS20', 3, 7, 'KWBC', header)

  call expect_eq_char(header(1), 'F', 'position 1')
  call expect_eq_char(header(2), 'T', 'position 2')
  call expect_eq_char(header(3), 'U', 'position 3')
  call expect_eq_char(header(4), 'S', 'position 4')
  call expect_eq_char(header(5), '2', 'position 5')
  call expect_eq_char(header(6), '0', 'position 6')
  call expect_eq_char(header(7), char(32), 'position 7 blank separator')
  call expect_eq_char(header(8), 'K', 'position 8')
  call expect_eq_char(header(9), 'W', 'position 9')
  call expect_eq_char(header(10), 'B', 'position 10')
  call expect_eq_char(header(11), 'C', 'position 11')
  call expect_eq_char(header(12), char(32), 'position 12 blank separator')
  call expect_eq_char(header(13), '0', 'position 13 day tens')
  call expect_eq_char(header(14), '3', 'position 14 day ones')
  call expect_eq_char(header(15), '0', 'position 15 hour tens')
  call expect_eq_char(header(16), '7', 'position 16 hour ones')
  call expect_eq_char(header(17), '0', 'position 17')
  call expect_eq_char(header(18), '0', 'position 18')
  call expect_eq_char(header(19), char(13), 'position 19 carriage return')
  call expect_eq_char(header(20), char(13), 'position 20 carriage return')
  call expect_eq_char(header(21), char(10), 'position 21 line feed')

  print *, 'SUCCESS!'

contains

  subroutine expect_eq_char(actual, expected, message)
    character(len=1), intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected ichar=', ichar(expected), ' actual ichar=', ichar(actual)
      stop 1
    end if
  end subroutine expect_eq_char

end program test_makwmo