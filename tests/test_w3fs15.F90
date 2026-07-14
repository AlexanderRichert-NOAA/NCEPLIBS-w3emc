! Unit tests for w3fs15() packed date/hour updates across key branches.
program test_w3fs15
  implicit none

  call test_zero_tau_copies_input()
  call test_invalid_input_returns_zero()
  call test_forward_leap_day_rollover()
  call test_forward_next_year_rollover()
  call test_backward_previous_year_rollover()

  print *, 'SUCCESS!'

contains

  subroutine test_zero_tau_copies_input()
    character(len=1) :: idate(4), ndate(4)

    call set_date(idate, 23, 7, 14, 9)
    call w3fs15(idate, 0, ndate)

    call expect_date(ndate, 23, 7, 14, 9, 'zero tau copies input')
  end subroutine test_zero_tau_copies_input

  subroutine test_invalid_input_returns_zero()
    character(len=1) :: idate(4), ndate(4)

    call set_date(idate, 23, 13, 14, 9)
    call w3fs15(idate, 4, ndate)

    call expect_date(ndate, 0, 0, 0, 0, 'invalid month returns zero date')
  end subroutine test_invalid_input_returns_zero

  subroutine test_forward_leap_day_rollover()
    character(len=1) :: idate(4), ndate(4)

    call set_date(idate, 96, 2, 28, 23)
    call w3fs15(idate, 2, ndate)

    call expect_date(ndate, 96, 2, 29, 1, '1996 leap day rollover')
  end subroutine test_forward_leap_day_rollover

  subroutine test_forward_next_year_rollover()
    character(len=1) :: idate(4), ndate(4)

    call set_date(idate, 99, 12, 31, 23)
    call w3fs15(idate, 2, ndate)

    call expect_date(ndate, 0, 1, 1, 1, 'rollover into year 2000')
  end subroutine test_forward_next_year_rollover

  subroutine test_backward_previous_year_rollover()
    character(len=1) :: idate(4), ndate(4)

    call set_date(idate, 1, 1, 1, 0)
    call w3fs15(idate, -1, ndate)

    call expect_date(ndate, 0, 12, 31, 23, 'backdate into year 1900')
  end subroutine test_backward_previous_year_rollover

  subroutine set_date(date_word, year, month, day, hour)
    character(len=1), intent(out) :: date_word(4)
    integer, intent(in) :: year, month, day, hour

    date_word(1) = achar(year)
    date_word(2) = achar(month)
    date_word(3) = achar(day)
    date_word(4) = achar(hour)
  end subroutine set_date

  subroutine expect_date(actual, year, month, day, hour, message)
    character(len=1), intent(in) :: actual(4)
    integer, intent(in) :: year, month, day, hour
    character(len=*), intent(in) :: message

    call expect_eq(iachar(actual(1)), year, trim(message) // ' year')
    call expect_eq(iachar(actual(2)), month, trim(message) // ' month')
    call expect_eq(iachar(actual(3)), day, trim(message) // ' day')
    call expect_eq(iachar(actual(4)), hour, trim(message) // ' hour')
  end subroutine expect_date

  subroutine expect_eq(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq

end program test_w3fs15