! Unit tests for w3fp11() title decoding and documented error returns.
program test_w3fp11
  implicit none

  call test_success_basic_forecast()
  call test_success_range_and_subcenter()
  call test_success_analysis_model_override()
  call test_err_missing_grib()
  call test_err_wrong_edition()
  call test_err_short_pds()
  call test_err_bad_table_version()
  call test_err_bad_indicator()
  call test_err_bad_level_type()
  call test_err_unknown_center()
  call test_err_unknown_subcenter_7()
  call test_err_unknown_subcenter_9()

  print *, 'SUCCESS!'

contains

  subroutine test_success_basic_forecast()
    character(len=8) :: ipds0
    character(len=28) :: ipds
    character(len=86) :: titl
    integer :: ierr

    call make_valid_inputs(ipds0, ipds)
    call w3fp11(ipds0, ipds, titl, ierr)

    call expect_eq_int(ierr, 0, 'basic forecast ierr')
    call expect_eq_str(titl(11:15), ' 500 ', 'basic forecast level value')
    call expect_eq_str(titl(16:20), 'ISBL ', 'basic forecast level type')
    call expect_trim_eq(titl(21:27), 'TMP', 'basic forecast parameter')
    call expect_eq_str(titl(29:32), '  6 ', 'basic forecast lead hour')
    call expect_eq_str(titl(34:36), 'HRS', 'basic forecast time units')
    call expect_eq_str(titl(55:57), 'MAR', 'basic forecast month')
    call expect_eq_str(titl(59:62), '2023', 'basic forecast year')
    call expect_trim_eq(titl(63:86), 'US  NWS - NCEP (WMC)', 'basic forecast center')
  end subroutine test_success_basic_forecast

  subroutine test_success_range_and_subcenter()
    character(len=8) :: ipds0
    character(len=28) :: ipds
    character(len=86) :: titl
    integer :: ierr

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 19, 3)
    call set_octet(ipds, 20, 6)
    call set_octet(ipds, 21, 2)
    call set_octet(ipds, 26, 8)
    call w3fp11(ipds0, ipds, titl, ierr)

    call expect_eq_int(ierr, 0, 'range/subcenter ierr')
    call expect_eq_str(titl(22:24), '  3', 'range lower bound')
    call expect_eq_str(titl(25:28), ' TO ', 'range separator')
    call expect_eq_str(titl(29:32), '  6 ', 'range upper bound')
    call expect_trim_eq(titl(63:86), 'AVIATION WEATHER CEN.', 'range/subcenter title tail')
  end subroutine test_success_range_and_subcenter

  subroutine test_success_analysis_model_override()
    character(len=8) :: ipds0
    character(len=28) :: ipds
    character(len=86) :: titl
    integer :: ierr

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 6, 10)
    call set_octet(ipds, 19, 0)
    call set_octet(ipds, 21, 0)
    call w3fp11(ipds0, ipds, titl, ierr)

    call expect_eq_int(ierr, 0, 'analysis override ierr')
    call expect_eq_str(titl(29:42), ' 00-HR FCST  ', 'analysis model override text')
  end subroutine test_success_analysis_model_override

  subroutine test_err_missing_grib()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    ipds0(1:4) = 'ABCD'
    call expect_ierr(ipds0, ipds, 1, 'missing GRIB marker')
  end subroutine test_err_missing_grib

  subroutine test_err_wrong_edition()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    ipds0(8:8) = char(2)
    call expect_ierr(ipds0, ipds, 2, 'wrong GRIB edition')
  end subroutine test_err_wrong_edition

  subroutine test_err_short_pds()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 3, 27)
    call expect_ierr(ipds0, ipds, 3, 'short PDS length')
  end subroutine test_err_short_pds

  subroutine test_err_bad_table_version()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 4, 132)
    call expect_ierr(ipds0, ipds, 9, 'unsupported parameter table version')
  end subroutine test_err_bad_table_version

  subroutine test_err_bad_indicator()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 9, 188)
    call expect_ierr(ipds0, ipds, 4, 'unknown parameter indicator')
  end subroutine test_err_bad_indicator

  subroutine test_err_bad_level_type()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 10, 99)
    call expect_ierr(ipds0, ipds, 5, 'unknown level type')
  end subroutine test_err_bad_level_type

  subroutine test_err_unknown_center()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 5, 1)
    call expect_ierr(ipds0, ipds, 6, 'unknown center')
  end subroutine test_err_unknown_center

  subroutine test_err_unknown_subcenter_7()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 26, 99)
    call expect_ierr(ipds0, ipds, 7, 'unknown NCEP subcenter')
  end subroutine test_err_unknown_subcenter_7

  subroutine test_err_unknown_subcenter_9()
    character(len=8) :: ipds0
    character(len=28) :: ipds

    call make_valid_inputs(ipds0, ipds)
    call set_octet(ipds, 5, 9)
    call set_octet(ipds, 26, 99)
    call expect_ierr(ipds0, ipds, 8, 'unknown RFC subcenter')
  end subroutine test_err_unknown_subcenter_9

  subroutine make_valid_inputs(ipds0, ipds)
    character(len=*), intent(out) :: ipds0, ipds

    ipds0 = achar(0)
    ipds = achar(0)

    ipds0(1:4) = 'GRIB'
    ipds0(8:8) = char(1)

    call set_octet(ipds, 1, 0)
    call set_octet(ipds, 2, 0)
    call set_octet(ipds, 3, 28)
    call set_octet(ipds, 4, 2)
    call set_octet(ipds, 5, 7)
    call set_octet(ipds, 6, 2)
    call set_octet(ipds, 9, 11)
    call set_octet(ipds, 10, 100)
    call set_octet(ipds, 11, 1)
    call set_octet(ipds, 12, 244)
    call set_octet(ipds, 13, 23)
    call set_octet(ipds, 14, 3)
    call set_octet(ipds, 15, 10)
    call set_octet(ipds, 16, 12)
    call set_octet(ipds, 18, 1)
    call set_octet(ipds, 19, 6)
    call set_octet(ipds, 20, 0)
    call set_octet(ipds, 21, 0)
    call set_octet(ipds, 25, 21)
    call set_octet(ipds, 26, 0)
  end subroutine make_valid_inputs

  subroutine set_octet(buffer, pos, value)
    character(len=*), intent(inout) :: buffer
    integer, intent(in) :: pos, value

    buffer(pos:pos) = char(value)
  end subroutine set_octet

  subroutine expect_ierr(ipds0, ipds, expected, message)
    character(len=*), intent(in) :: ipds0, ipds
    integer, intent(in) :: expected
    character(len=*), intent(in) :: message
    character(len=86) :: titl
    integer :: ierr

    titl = ' '
    call w3fp11(ipds0, ipds, titl, ierr)
    call expect_eq_int(ierr, expected, message)
  end subroutine expect_ierr

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

  subroutine expect_eq_str(actual, expected, message)
    character(len=*), intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected="', expected, '" actual="', actual, '"'
      stop 2
    end if
  end subroutine expect_eq_str

  subroutine expect_trim_eq(actual, expected, message)
    character(len=*), intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (trim(adjustl(actual)) /= expected) then
      print *, 'FAIL: ', trim(message), ' expected="', expected, '" actual="', trim(adjustl(actual)), '"'
      stop 3
    end if
  end subroutine expect_trim_eq

end program test_w3fp11