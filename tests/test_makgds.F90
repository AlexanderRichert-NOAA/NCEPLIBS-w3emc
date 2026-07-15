! Unit tests for makgds() GDS pack/unpack paths.
program test_makgds
  implicit none

  call test_gridid_pack_then_unpack()
  call test_pack_from_kgds_matches_direct_gds()
  call test_invalid_gridid_returns_error()

  print *, 'SUCCESS!'

contains

  subroutine test_gridid_pack_then_unpack()
    integer :: kgds_from_id(200), kgds_unpacked(200)
    character(len=1) :: gds(400)
    integer :: lengds, iret

    kgds_from_id = -999
    kgds_unpacked = -999
    gds = char(0)
    lengds = -1
    iret = -1

    ! Build GDS and KGDS from a known valid NCEP grid id.
    call makgds(2, kgds_from_id, gds, lengds, iret)
    call expect_eq_int(iret, 0, 'grid id 2 should succeed')
    call expect_true(lengds > 0, 'grid id 2 should produce nonzero GDS length')

    call makgds(-1, kgds_unpacked, gds, lengds, iret)
    call expect_eq_int(iret, 0, 'unpack path should succeed')

    call expect_kgds_equal(kgds_unpacked, kgds_from_id, 22, 'unpacked KGDS should match packed KGDS (first 22)')
  end subroutine test_gridid_pack_then_unpack

  subroutine test_pack_from_kgds_matches_direct_gds()
    integer :: kgds(200)
    character(len=1) :: gds_from_id(400), gds_from_kgds(400)
    integer :: len_from_id, len_from_kgds, iret

    kgds = -999
    gds_from_id = char(0)
    gds_from_kgds = char(0)
    len_from_id = -1
    len_from_kgds = -1
    iret = -1

    call makgds(2, kgds, gds_from_id, len_from_id, iret)
    call expect_eq_int(iret, 0, 'grid id 2 should succeed before IOPT=255')

    call makgds(255, kgds, gds_from_kgds, len_from_kgds, iret)
    call expect_eq_int(iret, 0, 'IOPT=255 should pack from KGDS')
    call expect_eq_int(len_from_kgds, len_from_id, 'IOPT=255 should preserve GDS length')
    call expect_gds_equal(gds_from_kgds, gds_from_id, len_from_id, 'IOPT=255 GDS should match direct packed GDS')
  end subroutine test_pack_from_kgds_matches_direct_gds

  subroutine test_invalid_gridid_returns_error()
    integer :: kgds(200)
    character(len=1) :: gds(400)
    integer :: lengds, iret

    kgds = 0
    gds = char(0)
    lengds = 0
    iret = 0

    ! Grid id 7 is not defined by W3FI71 and should fail in the IOPT>0 path.
    call makgds(7, kgds, gds, lengds, iret)
    call expect_true(iret /= 0, 'invalid grid id should return nonzero IRET')
  end subroutine test_invalid_gridid_returns_error

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

  subroutine expect_true(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, 'FAIL: ', trim(message)
      stop 1
    end if
  end subroutine expect_true

  subroutine expect_gds_equal(actual, expected, n, message)
    character(len=1), intent(in) :: actual(400), expected(400)
    integer, intent(in) :: n
    character(len=*), intent(in) :: message
    integer :: i

    do i = 1, n
      if (actual(i) /= expected(i)) then
        print *, 'FAIL: ', trim(message), ' index=', i
        print *, ' expected ichar=', ichar(expected(i)), ' actual ichar=', ichar(actual(i))
        stop 1
      end if
    end do
  end subroutine expect_gds_equal

  subroutine expect_kgds_equal(actual, expected, n, message)
    integer, intent(in) :: actual(200), expected(200)
    integer, intent(in) :: n
    character(len=*), intent(in) :: message
    integer :: i

    do i = 1, n
      if (actual(i) /= expected(i)) then
        print *, 'FAIL: ', trim(message), ' index=', i, ' expected=', expected(i), ' actual=', actual(i)
        stop 1
      end if
    end do
  end subroutine expect_kgds_equal

end program test_makgds
