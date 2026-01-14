# Script to generate Fortran test source from template
# This is called by CMake at build time to process the procedure list

# Read the procedure list file
file(STRINGS ${PROCEDURE_LIST_FILE} PROC_LIST)

# Build the use statement with all procedures
set(PROC_USE_STATEMENT "")
set(first_proc TRUE)
foreach(proc ${PROC_LIST})
  string(STRIP "${proc}" proc)
  if(NOT "${proc}" STREQUAL "")
    if(first_proc)
      set(PROC_USE_STATEMENT "${proc} &")
      set(first_proc FALSE)
    else()
      set(PROC_USE_STATEMENT "${PROC_USE_STATEMENT}\n    , ${proc} &")
    endif()
  endif()
endforeach()

# Remove trailing " &" from the last procedure
string(REGEX REPLACE " &$" "" PROC_USE_STATEMENT "${PROC_USE_STATEMENT}")

# Configure the template file to generate the Fortran source
configure_file(
  ${TEMPLATE_FILE}
  ${OUTPUT_FILE}
  @ONLY
)
