program test_fparser
#ifdef USE_W3EMC_MODULE
#  ifdef KIND_4
#    define W3EMC_MODULE w3emc_4
#  elif defined(KIND_D)
#    define W3EMC_MODULE w3emc_d
#  elif defined(KIND_8)
#    define W3EMC_MODULE w3emc_8
#  endif
  use W3EMC_MODULE, only: fparser
#endif
  implicit none
  character(25) :: carg
  integer :: marg
  real :: rarg(4)
  real, parameter :: tinyreal=tiny(1.0)

  rarg = -999.9
  carg = '867530.9,123456.7 09.8 123.4'
  marg = 3
  call fparser(carg, marg, rarg)
  if (any((rarg-(/867530.9,123456.7,9.8,-999.9/)).gt.tinyreal)) stop 1
end program
