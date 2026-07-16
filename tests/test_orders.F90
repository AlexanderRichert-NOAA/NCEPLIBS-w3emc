! Unit tests for ORDERS / ORDER4 / ORDEC4 / ORDEC8 in orders.f.
! Six test cases cover: 4-byte char sort (fresh + keep-idx), 4-byte integer sort
! (negative values exercise the bias path), 4-byte real sort, 8-byte char sort,
! 8-byte integer sort (main ORDERS body), and 8-byte real sort.
program test_orders
  implicit none

  call test_char4()
  call test_int4()
  call test_real4()
  call test_char8()
  call test_int8()
  call test_real8()

  print *, 'SUCCESS!'

contains

  ! ORDEC4: 4-byte character radix sort.
  ! Exercises IN=0 (fresh indexes) and IN=10 (keep indexes, idempotent on sorted idx).
  subroutine test_char4()
    integer, parameter :: N = 5
    character(4) :: keys(1, N)
    integer :: isort(N), idx(N)

    keys(1,1) = 'EEEE'; keys(1,2) = 'CCCC'; keys(1,3) = 'AAAA'
    keys(1,4) = 'DDDD'; keys(1,5) = 'BBBB'

    call ordec4(0, isort, keys, idx, N, 1, 4, 0)
    ! Sorted: AAAA(3), BBBB(5), CCCC(2), DDDD(4), EEEE(1)
    if (idx(1)/=3 .or. idx(2)/=5 .or. idx(3)/=2 .or. &
        idx(4)/=4 .or. idx(5)/=1) then
      print *, 'FAIL test_char4 fresh: idx=', idx
      stop 1
    end if

    ! IN=10: keep indexes as-is and re-sort (idempotent when already sorted)
    call ordec4(10, isort, keys, idx, N, 1, 4, 0)
    if (idx(1)/=3 .or. idx(2)/=5 .or. idx(3)/=2 .or. &
        idx(4)/=4 .or. idx(5)/=1) then
      print *, 'FAIL test_char4 keep-idx: idx=', idx
      stop 1
    end if
  end subroutine test_char4

  ! ORDER4 integer path: negative values force the bias computation branch.
  ! Also verifies the array is de-biased before returning.
  subroutine test_int4()
    integer, parameter :: N = 6
    integer(4) :: keys(1, N)
    integer :: isort(N), idx(N)

    keys(1,1) = -5_4;  keys(1,2) = 100_4; keys(1,3) = -100_4
    keys(1,4) = 0_4;   keys(1,5) = 50_4;  keys(1,6) = -1_4

    call order4(1, isort, keys, idx, N, 1, 4, 0)
    ! Sorted: -100(3), -5(1), -1(6), 0(4), 50(5), 100(2)
    if (idx(1)/=3 .or. idx(2)/=1 .or. idx(3)/=6 .or. &
        idx(4)/=4 .or. idx(5)/=5 .or. idx(6)/=2) then
      print *, 'FAIL test_int4 order: idx=', idx
      stop 1
    end if
    ! Verify bias was correctly undone
    if (keys(1,1)/=-5_4 .or. keys(1,3)/=-100_4 .or. keys(1,6)/=-1_4) then
      print *, 'FAIL test_int4 de-bias: keys=', keys
      stop 1
    end if
  end subroutine test_int4

  ! ORDER4 real path (ITYPE=2): negative reals force the real-bias path.
  subroutine test_real4()
    integer, parameter :: N = 4
    real(4) :: rkeys(1, N)
    integer :: isort(N), idx(N)

    rkeys(1,1) = 3.14_4; rkeys(1,2) = -2.71_4
    rkeys(1,3) = 0.0_4;  rkeys(1,4) = 1.0_4

    call order4(2, isort, rkeys, idx, N, 1, 4, 0)
    ! Sorted: -2.71(2), 0.0(3), 1.0(4), 3.14(1)
    if (idx(1)/=2 .or. idx(2)/=3 .or. idx(3)/=4 .or. idx(4)/=1) then
      print *, 'FAIL test_real4: idx=', idx
      stop 1
    end if
  end subroutine test_real4

  ! ORDEC8: 8-byte character radix sort (called via ORDERS with I1=8, IN=0).
  subroutine test_char8()
    integer, parameter :: N = 4
    character(8) :: keys(1, N)
    integer :: isort(N), idx(N)

    keys(1,1) = 'ZZZZZZZZ'; keys(1,2) = 'AAAAAAAA'
    keys(1,3) = 'MMMMMMMM'; keys(1,4) = 'BBBBBBBB'

    call ordec8(0, isort, keys, idx, N, 1, 8, 0)
    ! Sorted: AAAAAAAA(2), BBBBBBBB(4), MMMMMMMM(3), ZZZZZZZZ(1)
    if (idx(1)/=2 .or. idx(2)/=4 .or. idx(3)/=3 .or. idx(4)/=1) then
      print *, 'FAIL test_char8: idx=', idx
      stop 1
    end if
  end subroutine test_char8

  ! ORDERS with I1=8, IN=1: exercises the main ORDERS body integer bias path.
  ! Negative values ensure the SMAL bias computation runs.
  subroutine test_int8()
    integer, parameter :: N = 5
    integer(8) :: keys(1, N)
    integer :: isort(N), idx(N)

    keys(1,1) = 300_8;   keys(1,2) = -50_8;  keys(1,3) = 0_8
    keys(1,4) = 9999_8;  keys(1,5) = -1000_8

    call orders(1, isort, keys, idx, N, 1, 8, 0)
    ! Sorted: -1000(5), -50(2), 0(3), 300(1), 9999(4)
    if (idx(1)/=5 .or. idx(2)/=2 .or. idx(3)/=3 .or. &
        idx(4)/=1 .or. idx(5)/=4) then
      print *, 'FAIL test_int8 order: idx=', idx
      stop 1
    end if
    ! Verify bias was correctly undone
    if (keys(1,5)/=-1000_8 .or. keys(1,2)/=-50_8) then
      print *, 'FAIL test_int8 de-bias: keys=', keys
      stop 1
    end if
  end subroutine test_int8

  ! ORDERS with I1=8, IN=2: exercises the main ORDERS body real bias path.
  ! Negative reals ensure SMAL bias is applied.
  subroutine test_real8()
    integer, parameter :: N = 4
    real(8) :: rkeys(1, N)
    integer :: isort(N), idx(N)

    rkeys(1,1) = 1.5d0;  rkeys(1,2) = -3.0d0
    rkeys(1,3) = 0.0d0;  rkeys(1,4) = 100.0d0

    call orders(2, isort, rkeys, idx, N, 1, 8, 0)
    ! Sorted: -3.0(2), 0.0(3), 1.5(1), 100.0(4)
    if (idx(1)/=2 .or. idx(2)/=3 .or. idx(3)/=1 .or. idx(4)/=4) then
      print *, 'FAIL test_real8: idx=', idx
      stop 1
    end if
  end subroutine test_real8

end program test_orders
