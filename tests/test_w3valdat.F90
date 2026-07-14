! This program tests the w3valdat() function for valid and invalid dates.
program test_w3valdat
  implicit none

  logical, external :: w3valdat

  call expect_true(w3valdat((/2024, 2, 29,    0, 12, 30, 45, 500/)), 'valid leap day UTC')
  call expect_true(w3valdat((/2024, 2, 29,  530, 23, 59, 59, 999/)), 'valid +0530 timezone and max time')
  call expect_true(w3valdat((/2024, 2, 29, -930,  0,  0,  0,   0/)), 'valid -0930 timezone')

  call expect_false(w3valdat((/2023, 2, 29,    0, 12, 30, 45, 500/)), 'invalid leap day on non-leap year')
  call expect_false(w3valdat((/2024, 4, 31,    0, 12, 30, 45, 500/)), 'invalid day of month')
  call expect_false(w3valdat((/2024, 13, 1,    0, 12, 30, 45, 500/)), 'invalid month')
  call expect_false(w3valdat((/2024, 2, 29,  545, 12, 30, 45, 500/)), 'invalid timezone minutes')
  call expect_false(w3valdat((/2024, 2, 29, 2430, 12, 30, 45, 500/)), 'invalid timezone hours')
  call expect_false(w3valdat((/2024, 2, 29,    0, 24,  0,  0,   0/)), 'invalid 24:00:00 time')

  print *, 'SUCCESS!'

contains

  subroutine expect_true(value, message)
    logical, intent(in) :: value
    character(len=*), intent(in) :: message

    if (.not. value) then
      print *, 'FAIL: ', trim(message)
      stop 1
    end if
  end subroutine expect_true

  subroutine expect_false(value, message)
    logical, intent(in) :: value
    character(len=*), intent(in) :: message

    if (value) then
      print *, 'FAIL: ', trim(message)
      stop 2
    end if
  end subroutine expect_false

end program test_w3valdat