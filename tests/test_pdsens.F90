! Unit tests for pdsens() ensemble PDS extension packing.
program test_pdsens
  implicit none

  integer, parameter :: KENS_N = 5
  integer, parameter :: KPROB_N = 2
  integer, parameter :: KCLUST_N = 16
  integer, parameter :: KMEMBR_N = 80

  integer :: kens_in(KENS_N), kprob_in(KPROB_N)
  integer :: kclust_in(KCLUST_N), kmembr_in(KMEMBR_N)
  real :: xprob_in(KPROB_N)
  integer :: i

  character*1 :: msga(100)
  real, parameter :: tol = 1.0e-6

  ! Initialize test inputs
  kens_in = (/ 11, 37, 73, 109, 145 /)
  kprob_in = (/ 2, 3 /)
  xprob_in = (/ 0.5, 0.9 /)
  kclust_in = (/ 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155, 165 /)
  ! kmembr is packed as 1 bit per element; packing/unpacking preserves only 0 or 1
  do i = 1, 80
    if (mod(i, 3) == 0 .or. mod(i, 5) == 0) then
      kmembr_in(i) = 1
    else
      kmembr_in(i) = 0
    end if
  end do

  ! Test 1: ILAST < 41 (no packing should occur)
  msga = char(0)
  call pdsens(kens_in, kprob_in, xprob_in, kclust_in, kmembr_in, 40, msga)
  if (msga(1) /= char(0)) stop 1  ! First byte should remain zero

  ! Test 2: Section 1 only (ILAST = 45, pack KENS)
  msga = char(0)
  call pdsens(kens_in, kprob_in, xprob_in, kclust_in, kmembr_in, 45, msga)
  call verify_section1(msga, kens_in)

  ! Test 3: Sections 1-2 (ILAST = 55, pack KENS + KPROB + XPROB)
  msga = char(0)
  call pdsens(kens_in, kprob_in, xprob_in, kclust_in, kmembr_in, 55, msga)
  call verify_sections12(msga, kens_in, kprob_in, xprob_in)

  ! Test 4: All sections (ILAST = 86)
  msga = char(0)
  call pdsens(kens_in, kprob_in, xprob_in, kclust_in, kmembr_in, 86, msga)
  call verify_all_sections(msga, kens_in, kprob_in, xprob_in, kclust_in, kmembr_in)

  ! Test 5: Verify ILAST > 86 is clamped to 86 (boundary test)
  call verify_all_sections(msga, kens_in, kprob_in, xprob_in, kclust_in, kmembr_in)

  print *, 'SUCCESS!'

contains

  subroutine verify_section1(msga, kens_in)
    character*1, intent(in) :: msga(100)
    integer, intent(in) :: kens_in(5)
    integer :: kens(5), kprob(2), kclust(16), kmembr(80)
    real :: xprob(2)

    kens = -999
    kprob = -999
    xprob = -999.0
    kclust = -999
    kmembr = -999
    call pdseup(kens, kprob, xprob, kclust, kmembr, 45, msga)
    if (any(kens /= kens_in)) stop 10
  end subroutine verify_section1

  subroutine verify_sections12(msga, kens_in, kprob_in, xprob_in)
    character*1, intent(in) :: msga(100)
    integer, intent(in) :: kens_in(5), kprob_in(2)
    real, intent(in) :: xprob_in(2)
    integer :: kens(5), kprob(2), kclust(16), kmembr(80)
    real :: xprob(2)
    real, parameter :: tol = 1.0e-6

    kens = -999
    kprob = -999
    xprob = -999.0
    kclust = -999
    kmembr = -999
    call pdseup(kens, kprob, xprob, kclust, kmembr, 55, msga)
    if (any(kens /= kens_in)) stop 20
    if (any(kprob /= kprob_in)) stop 21
    if (any(abs(xprob - xprob_in) > tol)) stop 22
  end subroutine verify_sections12

  subroutine verify_all_sections(msga, kens_in, kprob_in, xprob_in, kclust_in, kmembr_in)
    character*1, intent(in) :: msga(100)
    integer, intent(in) :: kens_in(5), kprob_in(2), kclust_in(16), kmembr_in(80)
    real, intent(in) :: xprob_in(2)
    integer :: kens(5), kprob(2), kclust(16), kmembr(80)
    real :: xprob(2)
    real, parameter :: tol = 1.0e-6

    kens = -999
    kprob = -999
    xprob = -999.0
    kclust = -999
    kmembr = -999
    call pdseup(kens, kprob, xprob, kclust, kmembr, 86, msga)
    if (any(kens /= kens_in)) stop 30
    if (any(kprob /= kprob_in)) stop 31
    if (any(abs(xprob - xprob_in) > tol)) stop 32
    if (any(kclust /= kclust_in)) stop 33
    if (any(kmembr /= kmembr_in)) stop 34
  end subroutine verify_all_sections

end program test_pdsens
