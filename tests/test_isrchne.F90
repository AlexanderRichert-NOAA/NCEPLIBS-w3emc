! Unit tests for ISRCHNE in isrchne.f.
! Tests: N <= 0 (returns 0), found at start/middle/end, all equal (returns N+1),
! and non-unit increments (INCX > 1 and INCX < 0).
program test_isrchne
  implicit none

  interface
    integer function isrchne(n, x, incx, target)
      integer :: n, incx, target
      integer :: x(*)
    end function isrchne
  end interface

  call test_n_le_zero()
  call test_found_at_start()
  call test_found_in_middle()
  call test_not_found_all_equal()
  call test_incx_positive_stride()
  call test_incx_negative()

  print *, 'SUCCESS!'

contains

  ! Test N <= 0: should always return 0.
  subroutine test_n_le_zero()
    integer :: result
    integer :: x(5) = [1, 2, 3, 4, 5]

    result = isrchne(0, x, 1, 3)
    call expect_eq_int(result, 0, 'N=0 returns 0')

    result = isrchne(-1, x, 1, 3)
    call expect_eq_int(result, 0, 'N<0 returns 0')
  end subroutine test_n_le_zero

  ! Test found at first position: first element != target, return 1.
  subroutine test_found_at_start()
    integer :: result
    integer :: x(5), y(5)

    x = [9, 9, 9, 9, 9]

    result = isrchne(5, x, 1, 9)
    call expect_eq_int(result, 6, 'all equal target returns N+1')

    y = [7, 9, 9, 9, 9]
    result = isrchne(5, y, 1, 9)
    call expect_eq_int(result, 1, 'found not-equal at position 1')
  end subroutine test_found_at_start

  ! Test found in middle: search for first element != target.
  subroutine test_found_in_middle()
    integer :: result
    integer :: x(5)

    x = [5, 5, 5, 7, 5]

    result = isrchne(5, x, 1, 5)
    call expect_eq_int(result, 4, 'found not-equal at position 4')
  end subroutine test_found_in_middle

  ! Test not found (all equal to target): should return N+1.
  subroutine test_not_found_all_equal()
    integer :: result
    integer :: x(3)

    x = [2, 2, 2]

    result = isrchne(3, x, 1, 2)
    call expect_eq_int(result, 4, 'all equal returns N+1')
  end subroutine test_not_found_all_equal

  ! Test INCX > 1 (stride): skip elements to reach target.
  subroutine test_incx_positive_stride()
    integer :: result
    integer :: x(7), y(7), z(7)

    x = [9, 3, 9, 7, 9, 5, 9]

    ! INCX=2: examine positions 1, 3, 5, 7 (values 9, 9, 9, 9)
    result = isrchne(4, x, 2, 9)
    call expect_eq_int(result, 5, 'INCX=2 all equal returns N+1')

    y = [9, 3, 9, 7, 9, 5, 9]
    ! INCX=2: examine positions 1, 3, 5, 7 (values 9, 9, 9, 9)
    ! Try with target 7 at a skipped position; should return N+1
    result = isrchne(4, y, 2, 9)
    call expect_eq_int(result, 5, 'INCX=2 skips elements')

    z = [8, 3, 9, 7, 9, 5, 9]
    ! INCX=2: examine positions 1, 3, 5, 7 (values 8, 9, 9, 9)
    ! First element (8) != target (9), return 1
    result = isrchne(4, z, 2, 9)
    call expect_eq_int(result, 1, 'INCX=2 found at first position')
  end subroutine test_incx_positive_stride

  ! Test INCX < 0 (negative stride): search backwards.
  subroutine test_incx_negative()
    integer :: result
    integer :: x(5), y(5)

    x = [5, 5, 5, 5, 5]

    ! INCX=-1: starts at position 5, searches backwards
    ! 5 == target, continue; 5 == target, continue, etc. All equal, return N+1.
    result = isrchne(5, x, -1, 5)
    call expect_eq_int(result, 6, 'INCX=-1 all equal returns N+1')

    y = [5, 5, 5, 5, 7]
    ! INCX=-1: starts at position 5, searches backwards (values 7, 5, 5, 5, 5)
    ! 7 != target (5), return 1 (first iteration)
    result = isrchne(5, y, -1, 5)
    call expect_eq_int(result, 1, 'INCX=-1 found at logical position 1')
  end subroutine test_incx_negative

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

end program test_isrchne
