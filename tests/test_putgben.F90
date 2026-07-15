! Unit tests for putgben() packing and write behavior with bit control and ensemble support.
program test_putgben
  implicit none

  integer, parameter :: kf = 6

  ! Test 1: all valid data, auto bit detection (NBITS=0, IBS=0)
  call run_case(.false., 'all_valid_autobit', &
    (/ .true._1, .true._1, .true._1, .true._1, .true._1, .true._1 /), &
    ibs=0, nbits=0, use_ensemble=.false., expect_ok=.true., stop_base=1)

  ! Test 2: mixed bitmap, fixed bit width (NBITS=8)
  call run_case(.true., 'mixed_fixedbit', &
    (/ .true._1, .false._1, .true._1, .false._1, .true._1, .true._1 /), &
    ibs=0, nbits=8, use_ensemble=.false., expect_ok=.true., stop_base=2)

  ! Test 3: ensemble metadata with auto bit detection
  call run_case(.false., 'ensemble_autobit', &
    (/ .true._1, .true._1, .true._1, .true._1, .true._1, .true._1 /), &
    ibs=0, nbits=0, use_ensemble=.true., expect_ok=.true., stop_base=3)

  ! Test 4: all invalid data (KBM=0 case), auto bit detection zeros data, fails to pack
  call run_case(.true., 'all_invalid_kbm0', &
    (/ .false._1, .false._1, .false._1, .false._1, .false._1, .false._1 /), &
    ibs=0, nbits=0, use_ensemble=.false., expect_ok=.false., stop_base=4)

  print *, 'SUCCESS!'

contains

  subroutine run_case(use_bitmap, label, lb_in, ibs, nbits, use_ensemble, expect_ok, stop_base)
    logical, intent(in) :: use_bitmap
    character(len=*), intent(in) :: label
    logical*1, intent(in) :: lb_in(kf)
    integer, intent(in) :: ibs, nbits
    logical, intent(in) :: use_ensemble, expect_ok
    integer, intent(in) :: stop_base

    integer :: kpds(200), kgds(200), kens(200)
    real :: f_in(kf)
    integer :: i, iret, iret_io, lugb
    character(len=64) :: filename

    call init_test_record(kpds, kgds, kens, f_in)

    if (use_bitmap) then
      kpds(4) = 192
    else
      kpds(4) = 128
    end if

    if (use_ensemble) then
      kpds(23) = 2
      kpds(24) = 29
      kens(1) = 1
      kens(2) = 1
      kens(3) = 1
      kens(4) = 1
      kens(5) = 0
    end if

    do i = 1, kf
      if (.not. lb_in(i)) f_in(i) = -9999.0
    end do

    write(filename, '("test_putgben_",A,"_k",I0,".grb")') trim(label), kind(1.0)

    lugb = 31
    call baopenwt(lugb, trim(filename), iret_io)
    if (iret_io /= 0) stop (100 + stop_base)

    call putgben(lugb, kf, kpds, kgds, kens, ibs, nbits, lb_in, f_in, iret)
    if (expect_ok) then
      if (iret /= 0) stop (110 + stop_base)
    else
      if (iret == 0) stop (111 + stop_base)
    end if

    call baclose(lugb, iret_io)
    if (iret_io /= 0) stop (120 + stop_base)
  end subroutine run_case

  subroutine init_test_record(kpds, kgds, kens, f)
    integer, intent(out) :: kpds(200), kgds(200), kens(200)
    real, intent(out) :: f(kf)

    kpds = 0
    kgds = 0
    kens = 0

    ! Minimal, valid metadata for a tiny lat/lon grid
    kpds(1) = 7
    kpds(2) = 2
    kpds(3) = 255
    kpds(5) = 11
    kpds(6) = 100
    kpds(7) = 500
    kpds(8) = 24
    kpds(9) = 5
    kpds(10) = 17
    kpds(11) = 6
    kpds(12) = 30
    kpds(13) = 1
    kpds(14) = 12
    kpds(15) = 18
    kpds(16) = 10
    kpds(17) = 4
    kpds(19) = 2
    kpds(20) = 3
    kpds(21) = 21
    kpds(22) = 0
    kpds(23) = 1

    kgds(1) = 0
    kgds(2) = 3
    kgds(3) = 2
    kgds(4) = 40000
    kgds(5) = 50000
    kgds(6) = 128
    kgds(7) = 41000
    kgds(8) = 52000
    kgds(9) = 1000
    kgds(10) = 1000
    kgds(11) = 64
    kgds(19) = 0
    kgds(20) = 255

    f(1) = 1.0
    f(2) = 2.0
    f(3) = 3.0
    f(4) = 4.0
    f(5) = 5.0
    f(6) = 6.0
  end subroutine init_test_record

end program test_putgben
