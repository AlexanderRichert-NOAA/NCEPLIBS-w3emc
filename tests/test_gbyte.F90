program test_gbyte
  implicit none
  integer*4 :: iunpkd, ipackd
  real*4 :: rpackd
  character*4 :: kpackd

  equivalence(ipackd, rpackd, kpackd)

  call gbyte(99, iunpkd, 0, 1)
  if (iunpkd.ne.0) stop 10

  call gbyte(-99, iunpkd, 0, 1)
  if (iunpkd.ne.1) stop 12
  call gbyte(-99, iunpkd, 0, 32)
  if (iunpkd.ne.-99) stop 13

  rpackd = 999.9
  call gbyte(rpackd, iunpkd, 0, 1)
  if (iunpkd.ne.0) stop 20

  rpackd = -999.9
  call gbyte(rpackd, iunpkd, 0, 1)
  if (iunpkd.ne.1) stop 21

  kpackd = 'abc'
  call gbyte(kpackd, iunpkd, 0, 32)
  if (iunpkd.ne.543384161) stop 30

end program
