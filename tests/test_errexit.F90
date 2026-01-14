program test_errexit
#ifdef USE_W3EMC_MODULE
#ifdef KIND_4
  use w3emc_4
#elif defined(KIND_D)
  use w3emc_d
#elif defined(KIND_8)
  use w3emc_8
#endif
#endif
  implicit none
  character(len=1) :: iret_char
  integer :: iret
  call get_command_argument(1, iret_char)
  read(iret_char, '(i2)') iret
  call errexit(iret)
end program
