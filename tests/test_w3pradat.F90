! Unit tests for w3pradat() date/time string formatting.
program test_w3pradat
  implicit none

  call test_utc_formatting()
  call test_hour_only_timezone_formatting()
  call test_hour_minute_timezone_and_invalid_month()

  print *, 'SUCCESS!'

contains

  subroutine test_utc_formatting()
    integer :: idat(8)
    character(len=10) :: cdat(8)

    idat = (/2024, 2, 29, 0, 12, 34, 56, 7/)
    call w3pradat(idat, cdat)

    call expect_eq_trim(cdat(1), 'Thursday', 'utc day of week')
    call expect_eq_trim(cdat(2), 'February', 'utc month name')
    call expect_eq_trim(cdat(3), '29, 2024', 'utc day/year text')
    call expect_eq_trim(cdat(4), '2024-02-29', 'utc yyyy-mm-dd')
    call expect_eq_trim(cdat(5), '2024.060', 'utc yyyy.doy')
    call expect_eq_trim(cdat(6), '12:34:56', 'utc hh:mm:ss')
    call expect_eq_trim(cdat(7), '.007', 'utc milliseconds')
    call expect_eq_trim(cdat(8), 'UTC', 'utc zone label')
  end subroutine test_utc_formatting

  subroutine test_hour_only_timezone_formatting()
    integer :: idat(8)
    character(len=10) :: cdat(8)

    idat = (/2025, 1, 7, 500, 3, 4, 5, 89/)
    call w3pradat(idat, cdat)

    call expect_eq_trim(cdat(1), 'Tuesday', 'hour-only tz day of week')
    call expect_eq_trim(cdat(2), 'January', 'hour-only tz month name')
    call expect_eq_trim(adjustl(cdat(3)), '7, 2025', 'hour-only tz day/year text')
    call expect_eq_trim(cdat(4), '2025-01-07', 'hour-only tz yyyy-mm-dd')
    call expect_eq_trim(cdat(5), '2025.007', 'hour-only tz yyyy.doy')
    call expect_eq_trim(cdat(6), '03:04:05', 'hour-only tz hh:mm:ss')
    call expect_eq_trim(cdat(7), '.089', 'hour-only tz milliseconds')
    call expect_eq_trim(cdat(8), 'UTC+05h', 'hour-only tz zone label')
  end subroutine test_hour_only_timezone_formatting

  subroutine test_hour_minute_timezone_and_invalid_month()
    integer :: idat(8)
    character(len=10) :: cdat(8)

    idat = (/2024, 13, 1, -930, 23, 59, 59, 999/)
    call w3pradat(idat, cdat)

    call expect_eq_trim(cdat(2), '********', 'invalid month marker')
    call expect_eq_trim(cdat(4), '2024-13-01', 'invalid month still printed in yyyy-mm-dd')
    call expect_eq_trim(cdat(6), '23:59:59', 'hour-minute tz hh:mm:ss')
    call expect_eq_trim(cdat(7), '.999', 'hour-minute tz milliseconds')
    call expect_eq_trim(cdat(8), 'UTC-09h30m', 'hour-minute tz zone label')
  end subroutine test_hour_minute_timezone_and_invalid_month

  subroutine expect_eq_trim(actual, expected, message)
    character(len=*), intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (trim(actual) /= trim(expected)) then
      print *, 'FAIL: ', trim(message), ' expected="', trim(expected), '" actual="', trim(actual), '"'
      stop 1
    end if
  end subroutine expect_eq_trim

end program test_w3pradat