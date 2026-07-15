! Unit tests for putgb() packing and write behavior.
program test_putgb
  implicit none

  integer, parameter :: kf = 6

  call run_case(.false., 'none',       (/ .true._1, .true._1, .true._1, .true._1, .true._1, .true._1 /), .true., 1)
  call run_case(.true.,  'all_valid',  (/ .true._1, .true._1, .true._1, .true._1, .true._1, .true._1 /), .true., 2)
  call run_case(.true.,  'mixed',      (/ .true._1, .false._1, .true._1, .false._1, .true._1, .true._1 /), .true., 3)
  call run_case(.true.,  'all_missing',(/ .false._1, .false._1, .false._1, .false._1, .false._1, .false._1 /), .false., 4)

  print *, 'SUCCESS!'

contains

  subroutine run_case(use_bitmap, label, lb_in, expect_write_ok, stop_base)
    logical, intent(in) :: use_bitmap
    character(len=*), intent(in) :: label
    logical*1, intent(in) :: lb_in(kf)
    logical, intent(in) :: expect_write_ok
    integer, intent(in) :: stop_base

    integer :: kpds(200), kgds(200)
    integer :: jpds(200), jgds(200)
    integer :: kpds_out(200), kgds_out(200)
    real :: f_in(kf), f_out(kf)
    logical*1 :: lb_out(kf)
    integer :: i, iret, iret_io, k, kf_out, lugb
    logical :: expect_bitmap_out
    character(len=64) :: filename

    call init_test_record(kpds, kgds, f_in)
    if (use_bitmap) then
      kpds(4) = 192
    else
      kpds(4) = 128
    end if

    do i = 1, kf
      if (.not. lb_in(i)) f_in(i) = -9999.0
    end do

    write(filename, '("test_putgb_",A,"_k",I0,".grb")') trim(label), kind(1.0)

    lugb = 31
    call baopenwt(lugb, trim(filename), iret_io)
    if (iret_io /= 0) stop (100 + stop_base)

    call putgb(lugb, kf, kpds, kgds, lb_in, f_in, iret)
    if (expect_write_ok) then
      if (iret /= 0) stop (110 + stop_base)
    else
      if (iret == 0) stop (111 + stop_base)
    end if

    call baclose(lugb, iret_io)
    if (iret_io /= 0) stop (120 + stop_base)

    if (.not. expect_write_ok) return

    call baopenr(lugb, trim(filename), iret_io)
    if (iret_io /= 0) stop (130 + stop_base)

    jpds = -1
    jgds = -1
    kpds_out = 0
    kgds_out = 0
    f_out = -7777.0
    lb_out = .false.

    call getgb(lugb, 0, kf, -1, jpds, jgds, kf_out, k, kpds_out, kgds_out, lb_out, f_out, iret)
    if (iret /= 0) stop (140 + stop_base)
    if (kf_out /= kf) stop (150 + stop_base)

    expect_bitmap_out = use_bitmap .and. any(.not. lb_in)
    if (expect_bitmap_out) then
      if (kpds_out(4) /= 192) stop (160 + stop_base)
    else
      if (kpds_out(4) /= 128) stop (170 + stop_base)
    end if

    do i = 1, kf
      if (use_bitmap) then
        if (lb_out(i) .neqv. lb_in(i)) stop (180 + stop_base)
        if (lb_in(i)) then
          if (abs(f_out(i) - f_in(i)) > 1.0e-5) stop (190 + stop_base)
        end if
      else
        if (.not. lb_out(i)) stop (200 + stop_base)
        if (abs(f_out(i) - f_in(i)) > 1.0e-5) stop (210 + stop_base)
      end if
    end do

    call baclose(lugb, iret_io)
    if (iret_io /= 0) stop (220 + stop_base)
  end subroutine run_case

  subroutine init_test_record(kpds, kgds, f)
    integer, intent(out) :: kpds(200), kgds(200)
    real, intent(out) :: f(kf)

    kpds = 0
    kgds = 0

    ! Minimal, valid metadata for a tiny lat/lon grid used by putgb.
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

end program test_putgb
