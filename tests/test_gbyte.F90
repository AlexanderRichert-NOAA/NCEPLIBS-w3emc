program test_gbyte
  implicit none
  integer*4 :: iunpkd, ipackd
  real*4 :: rpackd
  character*4 :: kpackd

  equivalence(ipackd, rpackd, kpackd)

  call gbyte(99, iunpkd, 0, 1)
  if (iunpkd.ne.0) stop 10
  call gbytes(99, iunpkd, 0, 1, 0, 1)
  if (iunpkd.ne.0) stop 11

  call gbyte(-99, iunpkd, 0, 1)
  if (iunpkd.ne.1) stop 20
  call gbytes(-99, iunpkd, 0, 1, 0, 1)
  if (iunpkd.ne.1) stop 21
  call gbyte(-99, iunpkd, 0, 32)
  if (iunpkd.ne.-99) stop 22
  call gbytes(-99, iunpkd, 0, 32, 0, 1)
  if (iunpkd.ne.-99) stop 23

  rpackd = 999.9
  call gbyte(rpackd, iunpkd, 0, 1)
  if (iunpkd.ne.0) stop 30
  call gbytes(rpackd, iunpkd, 0, 1, 0, 1)
  if (iunpkd.ne.0) stop 31

  rpackd = -999.9
  call gbyte(rpackd, iunpkd, 0, 1)
  if (iunpkd.ne.1) stop 40
  call gbytes(rpackd, iunpkd, 0, 1, 0, 1)
  if (iunpkd.ne.1) stop 41

  kpackd = 'abc'
  call gbyte(kpackd, iunpkd, 0, 32)
  if (iunpkd.ne.543384161) stop 50
  call gbytes(kpackd, iunpkd, 0, 32, 0, 1)
  if (iunpkd.ne.543384161) stop 51

end program
