program test_errexit
  character(len=1) :: iret_char
  integer :: iret
  call get_command_argument(1, iret_char)
  read(iret_char, '(i2)') iret
  call errexit(iret)
end program
