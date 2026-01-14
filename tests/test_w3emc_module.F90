!> @file
!> @brief Test w3emc module interface
!> @author Test
!>
!> This program tests that the w3emc module can be used
!> and that procedures are accessible through it.

program test_w3emc_module
#ifdef USE_W3EMC_MODULE
#  ifdef KIND_4
#    define W3EMC_MODULE w3emc_4
#  elif defined(KIND_D)
#    define W3EMC_MODULE w3emc_d
#  elif defined(KIND_8)
#    define W3EMC_MODULE w3emc_8
#  endif
  use W3EMC_MODULE, only: w3kind
#endif
  implicit none
  
  integer :: kindreal, kindint
  
  print *, 'Testing w3emc module interface'
  
  ! Test a simple subroutine call
  call w3kind(kindreal, kindint)
  
  print *, 'Real kind:', kindreal
  print *, 'Integer kind:', kindint
  print *, 'Test passed!'
  
end program test_w3emc_module
