! Unit tests for instrument() statistics initialization, accumulation, query, and reset behavior.
program test_instrument
  implicit none

  call test_initialization_and_zero_stats()
  call test_accumulation_and_query()
  call test_reset()

  print *, 'SUCCESS!'

contains

  subroutine test_initialization_and_zero_stats()
    integer :: kall
    real :: ttot, tmin, tmax

    ! First call initializes internal arrays for 3 monitored sections.
    call instrument(3, kall, ttot, tmin, tmax)

    ! Querying an untouched valid section should return zeros.
    call instrument(-2, kall, ttot, tmin, tmax)
    call expect_eq_int(kall, 0, 'untouched section call count')
    call expect_eq_real(ttot, 0.0, 'untouched section total time')
    call expect_eq_real(tmin, 0.0, 'untouched section min time')
    call expect_eq_real(tmax, 0.0, 'untouched section max time')
  end subroutine test_initialization_and_zero_stats

  subroutine test_accumulation_and_query()
    integer :: kall
    real :: ttot, tmin, tmax

    ! Exercise first and subsequent accumulation updates for section 1.
    call instrument(1, kall, ttot, tmin, tmax)
    call instrument(1, kall, ttot, tmin, tmax)

    ! Negative K queries stats without adding a new sample.
    call instrument(-1, kall, ttot, tmin, tmax)
    call expect_eq_int(kall, 2, 'section 1 call count after two updates')
    call expect_true(ttot >= 0.0, 'total time non-negative')
    call expect_true(tmin >= 0.0, 'min time non-negative')
    call expect_true(tmax >= 0.0, 'max time non-negative')
    call expect_true(tmax >= tmin, 'max time >= min time')
    call expect_true(ttot >= tmax, 'total time >= max time')
  end subroutine test_accumulation_and_query

  subroutine test_reset()
    integer :: kall
    real :: ttot, tmin, tmax

    call instrument(0, kall, ttot, tmin, tmax)
    call instrument(-1, kall, ttot, tmin, tmax)

    call expect_eq_int(kall, 0, 'call count after reset')
    call expect_eq_real(ttot, 0.0, 'total time after reset')
    call expect_eq_real(tmin, 0.0, 'min time after reset')
    call expect_eq_real(tmax, 0.0, 'max time after reset')
  end subroutine test_reset

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

  subroutine expect_eq_real(actual, expected, message)
    real, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 2
    end if
  end subroutine expect_eq_real

  subroutine expect_true(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, 'FAIL: ', trim(message)
      stop 3
    end if
  end subroutine expect_true

end program test_instrument