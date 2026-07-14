! Unit tests for pdseup() using pdsens() to construct controlled messages.
program test_pdseup
  implicit none

  integer, parameter :: KENS_N = 5
  integer, parameter :: KPROB_N = 2
  integer, parameter :: KCLUST_N = 16
  integer, parameter :: KMEMBR_N = 80

  integer :: kens_in(KENS_N), kprob_in(KPROB_N)
  integer :: kclust_in(KCLUST_N), kmembr_in(KMEMBR_N)
  real :: xprob_in(KPROB_N)

  integer :: kens(KENS_N), kprob(KPROB_N)
  integer :: kclust(KCLUST_N), kmembr(KMEMBR_N)
  real :: xprob(KPROB_N)

  character*1 :: msga(100)
  real, parameter :: tol = 1.0e-6

  call init_inputs(kens_in, kprob_in, xprob_in, kclust_in, kmembr_in)
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  msga = char(0)

  ! Build one full extension payload and reuse it for boundary checks.
  call pdsens(kens_in, kprob_in, xprob_in, kclust_in, kmembr_in, 86, msga)

  ! Full unpack.
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  call pdseup(kens, kprob, xprob, kclust, kmembr, 86, msga)
  if (any(kens /= kens_in)) stop 1
  if (any(kprob /= kprob_in)) stop 2
  if (any(kclust /= kclust_in)) stop 3
  if (any(kmembr /= kmembr_in)) stop 4
  if (any(abs(xprob - xprob_in) > tol)) stop 5

  ! Only section 1 should be unpacked.
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  call pdseup(kens, kprob, xprob, kclust, kmembr, 45, msga)
  if (any(kens /= kens_in)) stop 10
  if (any(kprob /= -999)) stop 11
  if (any(xprob /= -999.0)) stop 12
  if (any(kclust /= -999)) stop 13
  if (any(kmembr /= -999)) stop 14

  ! Sections 1 and 2 should be unpacked at the section-2 threshold.
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  call pdseup(kens, kprob, xprob, kclust, kmembr, 46, msga)
  if (any(kens /= kens_in)) stop 20
  if (any(kprob /= kprob_in)) stop 21
  if (any(abs(xprob - xprob_in) > tol)) stop 22
  if (any(kclust /= -999)) stop 23
  if (any(kmembr /= -999)) stop 24

  ! Sections 1, 2 and 3 should be unpacked at the section-3 threshold.
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  call pdseup(kens, kprob, xprob, kclust, kmembr, 61, msga)
  if (any(kens /= kens_in)) stop 30
  if (any(kprob /= kprob_in)) stop 31
  if (any(abs(xprob - xprob_in) > tol)) stop 32
  if (any(kclust /= kclust_in)) stop 33
  if (any(kmembr /= -999)) stop 34

  ! All sections should be unpacked at the section-4 threshold.
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  call pdseup(kens, kprob, xprob, kclust, kmembr, 77, msga)
  if (any(kens /= kens_in)) stop 35
  if (any(kprob /= kprob_in)) stop 36
  if (any(abs(xprob - xprob_in) > tol)) stop 37
  if (any(kclust /= kclust_in)) stop 38
  if (any(kmembr /= kmembr_in)) stop 39

  ! ILAST below section threshold should leave outputs unchanged.
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  call pdseup(kens, kprob, xprob, kclust, kmembr, 40, msga)
  if (any(kens /= -999)) stop 40
  if (any(kprob /= -999)) stop 41
  if (any(xprob /= -999.0)) stop 42
  if (any(kclust /= -999)) stop 43
  if (any(kmembr /= -999)) stop 44

  ! ILAST larger than message byte count should leave outputs unchanged.
  call init_outputs(kens, kprob, xprob, kclust, kmembr)
  call pdseup(kens, kprob, xprob, kclust, kmembr, 87, msga)
  if (any(kens /= -999)) stop 50
  if (any(kprob /= -999)) stop 51
  if (any(xprob /= -999.0)) stop 52
  if (any(kclust /= -999)) stop 53
  if (any(kmembr /= -999)) stop 54

  print *, 'SUCCESS!'

contains

  subroutine init_inputs(kens, kprob, xprob, kclust, kmembr)
    integer, intent(out) :: kens(:), kprob(:), kclust(:), kmembr(:)
    real, intent(out) :: xprob(:)
    integer :: i

    kens = (/ 11, 37, 73, 109, 145 /)
    kprob = (/ 19, 211 /)
    xprob = (/ 1.5, -7.75 /)
    do i = 1, size(kclust)
      kclust(i) = 90 + 3 * i
    end do
    do i = 1, size(kmembr)
      if (mod(i, 3) == 0 .or. mod(i, 5) == 0) then
        kmembr(i) = 1
      else
        kmembr(i) = 0
      end if
    end do
  end subroutine init_inputs

  subroutine init_outputs(kens, kprob, xprob, kclust, kmembr)
    integer, intent(out) :: kens(:), kprob(:), kclust(:), kmembr(:)
    real, intent(out) :: xprob(:)

    kens = -999
    kprob = -999
    xprob = -999.0
    kclust = -999
    kmembr = -999
  end subroutine init_outputs

end program test_pdseup