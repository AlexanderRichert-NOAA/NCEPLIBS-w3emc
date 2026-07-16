! Unit tests for idsdef() default decimal scaling table population.
program test_idsdef
  implicit none

  call test_supported_versions()
  call test_unsupported_versions_leave_array_unchanged()

  print *, 'SUCCESS!'

contains

  subroutine test_supported_versions()
    integer :: ids_v1(255), ids_v2(255)

    ids_v1 = -999
    ids_v2 = -999

    call idsdef(1, ids_v1)
    call idsdef(2, ids_v2)

    call expect_array_eq(ids_v1, ids_v2, 'IPTV=1 and IPTV=2 should produce identical defaults')

    call expect_eq_int(ids_v1(1), -1, 'IDS(1) pressure scaling')
    call expect_eq_int(ids_v1(3), 3, 'IDS(3) pressure tendency scaling')
    call expect_eq_int(ids_v1(11), 1, 'IDS(11) temperature scaling')
    call expect_eq_int(ids_v1(19), 4, 'IDS(19) lapse rate scaling')
    call expect_eq_int(ids_v1(32), 1, 'IDS(32) wind speed scaling')
    call expect_eq_int(ids_v1(39), 3, 'IDS(39) pressure vertical velocity scaling')
    call expect_eq_int(ids_v1(51), 4, 'IDS(51) specific humidity scaling')
    call expect_eq_int(ids_v1(66), 2, 'IDS(66) snow depth scaling')
    call expect_eq_int(ids_v1(111), 0, 'IDS(111) net solar flux scaling')
    call expect_eq_int(ids_v1(139), 11, 'IDS(139) PV mass-weighted scaling')
    call expect_eq_int(ids_v1(149), 10, 'IDS(149) potential vorticity scaling')
    call expect_eq_int(ids_v1(176), 2, 'IDS(176) latitude scaling')
    call expect_eq_int(ids_v1(183), 5, 'IDS(183) x-gradient height scaling')
    call expect_eq_int(ids_v1(214), 6, 'IDS(214) convective precipitation rate scaling')
    call expect_eq_int(ids_v1(224), 1, 'IDS(224) baseflow-groundwater runoff scaling')
    call expect_eq_int(ids_v1(225), 1, 'IDS(225) storm surface runoff scaling')
    call expect_eq_int(ids_v1(233), 0, 'IDS(233) upward total radiation flux scaling')
    call expect_eq_int(ids_v1(251), 7, 'IDS(251) longwave heating rate scaling')

    ! These entries are intentionally not assigned by idsdef() and must retain input values.
    call expect_eq_int(ids_v1(4), -999, 'IDS(4) untouched')
    call expect_eq_int(ids_v1(5), -999, 'IDS(5) untouched')
    call expect_eq_int(ids_v1(10), -999, 'IDS(10) untouched')
    call expect_eq_int(ids_v1(24), -999, 'IDS(24) untouched')
    call expect_eq_int(ids_v1(77), -999, 'IDS(77) untouched')
    call expect_eq_int(ids_v1(170), -999, 'IDS(170) untouched')
    call expect_eq_int(ids_v1(252), -999, 'IDS(252) untouched')
    call expect_eq_int(ids_v1(255), -999, 'IDS(255) untouched')
  end subroutine test_supported_versions

  subroutine test_unsupported_versions_leave_array_unchanged()
    integer :: ids(255)

    ids = -12345
    call idsdef(0, ids)
    call expect_all_eq(ids, -12345, 'IPTV=0 should not modify IDS')

    ids = 24680
    call idsdef(3, ids)
    call expect_all_eq(ids, 24680, 'IPTV=3 should not modify IDS')
  end subroutine test_unsupported_versions_leave_array_unchanged

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

  subroutine expect_array_eq(actual, expected, message)
    integer, intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    integer :: i

    if (size(actual) /= size(expected)) then
      print *, 'FAIL: ', trim(message), ' size mismatch'
      stop 2
    end if

    do i = 1, size(actual)
      if (actual(i) /= expected(i)) then
        print *, 'FAIL: ', trim(message), ' at index=', i, ' expected=', expected(i), ' actual=', actual(i)
        stop 3
      end if
    end do
  end subroutine expect_array_eq

  subroutine expect_all_eq(actual, expected, message)
    integer, intent(in) :: actual(:), expected
    character(len=*), intent(in) :: message
    integer :: i

    do i = 1, size(actual)
      if (actual(i) /= expected) then
        print *, 'FAIL: ', trim(message), ' at index=', i, ' expected=', expected, ' actual=', actual(i)
        stop 4
      end if
    end do
  end subroutine expect_all_eq

end program test_idsdef
