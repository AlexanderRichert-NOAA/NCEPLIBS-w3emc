program test_fparsei
#ifdef USE_W3EMC_MODULE
#  ifdef KIND_4
#    define W3EMC_MODULE w3emc_4
#  elif defined(KIND_D)
#    define W3EMC_MODULE w3emc_d
#  elif defined(KIND_8)
#    define W3EMC_MODULE w3emc_8
#  endif
  use W3EMC_MODULE, only: fparsei
#endif
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
