! This is a test in the NCEPLIBS-w3emc project.
!
! Test the summary subroutine.
!
! Kyle Gerheiser
program test_summary
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
  print *, "Testing summary()..."
  call start()
  call summary()
  print *, "SUCCESS"
end program test_summary

