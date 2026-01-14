! This program tests the xmovex() subroutine for several cases.
! Alex Richert, Apr 2025
PROGRAM TEST_XMOVEX
#ifdef USE_W3EMC_MODULE
#  ifdef KIND_4
#    define W3EMC_MODULE w3emc_4
#  elif defined(KIND_D)
#    define W3EMC_MODULE w3emc_d
#  elif defined(KIND_8)
#    define W3EMC_MODULE w3emc_8
#  endif
  use W3EMC_MODULE, only: xmovex
#endif
  IMPLICIT NONE
  
  ! Test cases
  CALL TEST_EMPTY_ARRAY()
  CALL TEST_SINGLE_CHAR()
  CALL TEST_MULTIPLE_CHARS()
  
  PRINT *, "All tests passed successfully!"
  
CONTAINS

  ! Test with zero bytes
  SUBROUTINE TEST_EMPTY_ARRAY()
    CHARACTER(LEN=1) :: IN_ARRAY(1), OUT_ARRAY(1)
    INTEGER :: IBYTES
    
    IN_ARRAY(1) = 'A'
    OUT_ARRAY(1) = 'Z'
    IBYTES = 0
    
    CALL XMOVEX(OUT_ARRAY, IN_ARRAY, IBYTES)
    
    ! No data should be copied when IBYTES=0
    IF (OUT_ARRAY(1) /= 'Z') THEN
      PRINT *, "TEST_EMPTY_ARRAY: Failed - Data was copied when IBYTES=0"
      STOP 1
    END IF
    
    PRINT *, "TEST_EMPTY_ARRAY: Passed"
  END SUBROUTINE TEST_EMPTY_ARRAY
  
  ! Test with a single character
  SUBROUTINE TEST_SINGLE_CHAR()
    CHARACTER(LEN=1) :: IN_ARRAY(1), OUT_ARRAY(1)
    INTEGER :: IBYTES
    
    IN_ARRAY(1) = 'X'
    OUT_ARRAY(1) = ' '
    IBYTES = 1
    
    CALL XMOVEX(OUT_ARRAY, IN_ARRAY, IBYTES)
    
    IF (OUT_ARRAY(1) /= 'X') THEN
      PRINT *, "TEST_SINGLE_CHAR: Failed - Character not copied correctly"
      PRINT *, "Expected: 'X', Got: '", OUT_ARRAY(1), "'"
      STOP 2
    END IF
    
    PRINT *, "TEST_SINGLE_CHAR: Passed"
  END SUBROUTINE TEST_SINGLE_CHAR
  
  ! Test with multiple characters
  SUBROUTINE TEST_MULTIPLE_CHARS()
    CHARACTER(LEN=1) :: IN_ARRAY(5), OUT_ARRAY(5)
    INTEGER :: IBYTES, I
    
    ! Initialize arrays
    DO I = 1, 5
      IN_ARRAY(I) = CHAR(IACHAR('A') + I - 1)  ! 'A', 'B', 'C', 'D', 'E'
      OUT_ARRAY(I) = '0'
    END DO
    
    IBYTES = 5
    
    CALL XMOVEX(OUT_ARRAY, IN_ARRAY, IBYTES)
    
    ! Verify all characters were copied correctly
    DO I = 1, 5
      IF (OUT_ARRAY(I) /= IN_ARRAY(I)) THEN
        PRINT *, "TEST_MULTIPLE_CHARS: Failed at position ", I
        PRINT *, "Expected: '", IN_ARRAY(I), "', Got: '", OUT_ARRAY(I), "'"
        STOP 3
      END IF
    END DO
    
    PRINT *, "TEST_MULTIPLE_CHARS: Passed"
  END SUBROUTINE TEST_MULTIPLE_CHARS
  
END PROGRAM TEST_XMOVEX
