# This is the CMake build file for the test directory of NCEPLIBS-w3emc.

# Kyle Gerheiser, Ed Hartnett, Alex Richert

execute_process(COMMAND cmake -E create_symlink
  "${CMAKE_CURRENT_SOURCE_DIR}/data"
  "${CMAKE_CURRENT_BINARY_DIR}/data"
)

function(create_test name kind)
  add_executable(${name}_${kind} ${name}.F90)
  target_link_libraries(${name}_${kind} PRIVATE w3emc_${kind})
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})  
endfunction()

function(create_test_sh name kind)
  add_executable(${name}_${kind} ${name}.F90)
  target_link_libraries(${name}_${kind} PRIVATE w3emc_${kind})
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})
  add_test(NAME ${name}_${kind} COMMAND ${CMAKE_SOURCE_DIR}/tests/${name}.sh ${kind})
endfunction()

function(create_test_w3as00 kind)
  add_executable(test_w3as00_${kind} test_w3as00.F90)
  target_link_libraries(test_w3as00_${kind} PRIVATE w3emc_${kind})
  set_target_properties(test_w3as00_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(test_w3as00_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  target_compile_definitions(test_w3as00_${kind} PUBLIC -DKIND_${kind})

  add_test(NAME test_w3as00_${kind}_noargs COMMAND test_w3as00_${kind})
  add_test(NAME test_w3as00_${kind}_len0 COMMAND test_w3as00_${kind} len0)
  add_test(NAME test_w3as00_${kind}_noparm COMMAND test_w3as00_${kind} noparm)
  add_test(NAME test_w3as00_${kind}_parm_ok COMMAND test_w3as00_${kind} parm_ok parm=abc)
  add_test(NAME test_w3as00_${kind}_parm_trunc COMMAND test_w3as00_${kind} parm_trunc PARM=ABCDEFGHIJK)
endfunction()

# These are the tests.
foreach(kind ${kinds})
  create_test(test_summary ${kind})
  create_test(test_mkfldsep ${kind})
  create_test(test_gbytec ${kind})
  create_test(test_w3tagb ${kind})
  create_test(test_w3fi71 ${kind})
#  create_test(test_w3fi72 ${kind})
  create_test(test_w3fi73 ${kind})
  create_test(test_w3fi74 ${kind})
  create_test(test_w3fi62 ${kind})
  create_test(test_fparsei ${kind})
  create_test(test_fparser ${kind})
  create_test(test_gbyte ${kind})
  create_test(test_pdseup ${kind})
  create_test(test_getgi ${kind})
  create_test(test_getgbemh ${kind})
  create_test_sh(test_errexit ${kind})
  create_test_sh(test_errmsg ${kind})
  create_test(test_w3fi63 ${kind})
  create_test(test_fi637 ${kind})
  create_test(test_xmovex ${kind})
  create_test(test_xstore ${kind})
  create_test(test_w3movdat ${kind})
  create_test(test_w3valdat ${kind})
  create_test(test_idsdef ${kind})
  create_test(test_w3fp11 ${kind})
  create_test(test_w3pradat ${kind})
  create_test(test_w3trnarg ${kind})
  create_test(test_w3fs15 ${kind})
  create_test_w3as00(${kind})
  create_test(test_w3kind ${kind})
  create_test(test_orders ${kind})
endforeach()  
