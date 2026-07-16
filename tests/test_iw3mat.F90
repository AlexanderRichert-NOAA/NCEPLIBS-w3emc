! Unit tests for iw3mat() equality checks across integer arrays.
program test_iw3mat
  implicit none

  interface
    logical function iw3mat(l1, l2, n)
      integer :: n
      integer :: l1(*)
      integer :: l2(*)
    end function iw3mat
  end interface

  call test_all_words_equal()
  call test_mismatch_returns_false()
  call test_zero_length_returns_true()

  print *, 'SUCCESS!'

contains

  subroutine test_all_words_equal()
    integer :: a(4), b(4)

    a = [11, 22, 33, 44]
    b = [11, 22, 33, 44]

    call expect_true(iw3mat(a, b, 4), 'identical arrays should match')
  end subroutine test_all_words_equal

  subroutine test_mismatch_returns_false()
    integer :: a(4), b(4)

    a = [11, 22, 33, 44]
    b = [11, 22, 99, 44]

    call expect_false(iw3mat(a, b, 4), 'mismatch should return false')
  end subroutine test_mismatch_returns_false

  subroutine test_zero_length_returns_true()
    integer :: a(2), b(2)

    a = [1, 2]
    b = [9, 8]

    call expect_true(iw3mat(a, b, 0), 'N=0 should return true')
  end subroutine test_zero_length_returns_true

  subroutine expect_true(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, 'FAIL: ', trim(message)
      stop 1
    end if
  end subroutine expect_true

  subroutine expect_false(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (condition) then
      print *, 'FAIL: ', trim(message)
      stop 2
    end if
  end subroutine expect_false

end program test_iw3mat