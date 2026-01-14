! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3tagb function.
!
! Kyle Gerheiser
program test_w3tagb
#ifdef USE_W3EMC_MODULE
#  ifdef KIND_4
#    define W3EMC_MODULE w3emc_4
#  elif defined(KIND_D)
#    define W3EMC_MODULE w3emc_d
#  elif defined(KIND_8)
#    define W3EMC_MODULE w3emc_8
#  endif
  use W3EMC_MODULE, only: w3tagb
#endif
  implicit none
  integer :: year, julian_day, hundreths_of_julian_day

  print *, "Testing w3tagb..."
  
  year = 2021
  julian_day = 21278
  hundreths_of_julian_day = 0

  ! prints information
  call w3tagb("test_w3tagb", year, julian_day, hundreths_of_julian_day, "emc")
  print *, "SUCCESS"
end program test_w3tagb
