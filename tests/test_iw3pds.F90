! Unit tests for iw3pds()/iw3pdb() PDS comparison masks.
program test_iw3pds
  implicit none

  logical, external :: iw3pds, iw3pdb

  call test_exact_matches()
  call test_key_masks()

  print *, 'SUCCESS!'

contains

  subroutine test_exact_matches()
    character(len=1) :: l1(24), l2(24)
    integer :: key

    call fill_sample_pds(l1)
    l2 = l1

    do key = 0, 3
      call expect_compare(.true., l1, l2, key, 'exact match for key')
    end do
  end subroutine test_exact_matches

  subroutine test_key_masks()
    call expect_single_byte_difference(2, 0, .false., 'key=0 compares bytes 1-3')
    call expect_single_byte_difference(2, 1, .false., 'key=1 compares bytes 1-3')
    call expect_single_byte_difference(2, 2, .false., 'key=2 compares bytes 1-3')
    call expect_single_byte_difference(2, 3, .false., 'key=3 compares bytes 1-3')

    call expect_single_byte_difference(4, 0, .true., 'key=0 ignores byte 4')
    call expect_single_byte_difference(4, 1, .true., 'key=1 ignores byte 4')
    call expect_single_byte_difference(4, 2, .true., 'key=2 ignores byte 4')
    call expect_single_byte_difference(4, 3, .true., 'key=3 ignores byte 4')

    call expect_single_byte_difference(5, 0, .false., 'key=0 compares byte 5')
    call expect_single_byte_difference(5, 1, .false., 'key=1 compares byte 5')
    call expect_single_byte_difference(5, 2, .false., 'key=2 compares byte 5')
    call expect_single_byte_difference(5, 3, .true., 'key=3 ignores bytes 5-6')

    call expect_single_byte_difference(7, 0, .false., 'key=0 compares byte 7')
    call expect_single_byte_difference(7, 1, .false., 'key=1 compares byte 7')
    call expect_single_byte_difference(7, 2, .false., 'key=2 compares byte 7')
    call expect_single_byte_difference(7, 3, .false., 'key=3 compares bytes 7-12')

    call expect_single_byte_difference(13, 0, .true., 'key=0 ignores date byte 13')
    call expect_single_byte_difference(13, 1, .false., 'key=1 compares date byte 13')
    call expect_single_byte_difference(13, 2, .true., 'key=2 ignores bytes 13-24')
    call expect_single_byte_difference(13, 3, .true., 'key=3 ignores bytes 13-24')

    call expect_single_byte_difference(18, 0, .false., 'key=0 compares byte 18')
    call expect_single_byte_difference(18, 1, .false., 'key=1 compares byte 18')
    call expect_single_byte_difference(18, 2, .true., 'key=2 ignores byte 18')
    call expect_single_byte_difference(18, 3, .true., 'key=3 ignores byte 18')
  end subroutine test_key_masks

  subroutine expect_single_byte_difference(index, key, expected, message)
    integer, intent(in) :: index, key
    logical, intent(in) :: expected
    character(len=*), intent(in) :: message
    character(len=1) :: l1(24), l2(24)

    call fill_sample_pds(l1)
    l2 = l1
    l2(index) = '#'

    call expect_compare(expected, l1, l2, key, message)
  end subroutine expect_single_byte_difference

  subroutine fill_sample_pds(pds)
    character(len=1), intent(out) :: pds(24)
    character(len=24), parameter :: seed = 'ABCDEFGHIJKLMNOPQRSTUVWX'
    integer :: i

    do i = 1, len(seed)
      pds(i) = seed(i:i)
    end do
  end subroutine fill_sample_pds

  subroutine expect_compare(expected, l1, l2, key, message)
    logical, intent(in) :: expected
    character(len=1), intent(in) :: l1(24), l2(24)
    integer, intent(in) :: key
    character(len=*), intent(in) :: message

    call expect_eq_logical(iw3pds(l1, l2, key), expected, trim(message) // ' (iw3pds)')
    call expect_eq_logical(iw3pdb(l1, l2, key), expected, trim(message) // ' (iw3pdb)')
  end subroutine expect_compare

  subroutine expect_eq_logical(actual, expected, message)
    logical, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual .neqv. expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_logical

end program test_iw3pds