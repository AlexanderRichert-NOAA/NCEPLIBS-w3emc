! Unit tests for w3as00() command-line PARM extraction behavior.
program test_w3as00
  implicit none

  integer :: argc
  character(len=32) :: scenario

  argc = command_argument_count()

  if (argc == 0) then
    call case_noargs()
  else
    call get_command_argument(1, scenario)
    select case (trim(scenario))
    case ('len0')
      call case_len0()
    case ('noparm')
      call case_noparm()
    case ('parm_ok')
      call case_parm_ok()
    case default
      print *, 'FAIL: unknown scenario "', trim(scenario), '"'
      stop 10
    end select
  end if

  print *, 'SUCCESS!'

contains

  subroutine case_len0()
    integer :: nch_parm, iret
    character(len=0) :: cparm

    call w3as00(nch_parm, cparm, iret)

    call expect_eq_int(iret, -1, 'len0 iret')
    call expect_eq_int(nch_parm, 0, 'len0 nch_parm')
  end subroutine case_len0

  subroutine case_noargs()
    integer :: nch_parm, iret
    character(len=16) :: cparm

    call w3as00(nch_parm, cparm, iret)

    call expect_eq_int(iret, 2, 'noargs iret')
    call expect_eq_int(nch_parm, 0, 'noargs nch_parm')
  end subroutine case_noargs

  subroutine case_noparm()
    integer :: nch_parm, iret
    character(len=16) :: cparm

    call w3as00(nch_parm, cparm, iret)

    call expect_eq_int(iret, 3, 'noparm iret')
    call expect_eq_int(nch_parm, 0, 'noparm nch_parm')
  end subroutine case_noparm

  subroutine case_parm_ok()
    integer :: nch_parm, iret
    character(len=16) :: cparm

    call w3as00(nch_parm, cparm, iret)

    call expect_eq_int(iret, 0, 'parm_ok iret')
    call expect_eq_int(nch_parm, 3, 'parm_ok nch_parm')
    call expect_eq_str(cparm(1:nch_parm), 'abc', 'parm_ok parsed value')
  end subroutine case_parm_ok

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

end program test_w3as00
