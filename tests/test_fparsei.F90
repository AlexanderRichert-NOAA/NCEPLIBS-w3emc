program test_fparsei
  implicit none
  character(25) :: carg
  integer :: marg
  integer :: karg(4)

  karg = -999
  carg = '8675309,1234567 098 1234'
  marg = 3
  call fparsei(carg, marg, karg)
  if (any(karg .ne. (/8675309,1234567,98,-999/))) stop 1
end program
