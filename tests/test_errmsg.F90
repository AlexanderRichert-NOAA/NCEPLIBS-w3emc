program test_errmsg
#ifdef USE_W3EMC_MODULE
#ifdef KIND_4
  use w3emc_4
#elif defined(KIND_D)
  use w3emc_d
#elif defined(KIND_8)
  use w3emc_8
#endif
#endif
  call errmsg("This is an error message.")
end program
