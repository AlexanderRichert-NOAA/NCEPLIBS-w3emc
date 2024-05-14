program test_fparsei
  implicit none
  character(25) :: carg
  integer :: marg
  real :: rarg(4)
  real, parameter :: tinyreal=tiny(1.0)

  rarg = -999.9
  carg = '867530.9,123456.7 09.8 123.4'
  marg = 3
  call fparsei(carg, marg, rarg)
  if (any((rarg-(/867530.9,123456.7,9.8,-999.9/)).gt.tinyreal)) stop 1
end program
