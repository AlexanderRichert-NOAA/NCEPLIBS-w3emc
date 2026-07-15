! Test suite for W3AI24 - String comparison function
! Tests matching strings and mismatches at different positions
PROGRAM TEST_W3AI24
  IMPLICIT NONE
  
  INTEGER :: status
  LOGICAL :: W3AI24
  
  status = 0
  
  ! Test 1: Both strings match completely
  CALL TEST_STRINGS_MATCH(status)
  
  ! Test 2: First character mismatch
  CALL TEST_FIRST_CHAR_MISMATCH(status)
  
  ! Test 3: Last character mismatch
  CALL TEST_LAST_CHAR_MISMATCH(status)
  
  IF (status == 0) THEN
    PRINT *, "All tests PASSED"
    STOP 0
  ELSE
    PRINT *, "Some tests FAILED"
    STOP 1
  END IF

CONTAINS

  ! Test: Both strings match - returns .TRUE.
  SUBROUTINE TEST_STRINGS_MATCH(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: str1(10), str2(10)
    INTEGER :: length
    LOGICAL :: result
    
    WRITE(*,*) "Test 1: Strings match completely"
    
    ! Setup: Create matching strings "HELLO"
    str1(1:5) = (/ 'H', 'E', 'L', 'L', 'O' /)
    str2(1:5) = (/ 'H', 'E', 'L', 'L', 'O' /)
    length = 5
    
    ! Call W3AI24
    result = W3AI24(str1, str2, length)
    
    ! Verify: Should return .TRUE.
    IF (.NOT. result) THEN
      WRITE(*,*) "  FAILED: Expected TRUE for matching strings, got FALSE"
      status = 1
      RETURN
    END IF
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_STRINGS_MATCH

  ! Test: First character mismatch - returns .FALSE.
  SUBROUTINE TEST_FIRST_CHAR_MISMATCH(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: str1(10), str2(10)
    INTEGER :: length
    LOGICAL :: result
    
    WRITE(*,*) "Test 2: First character mismatch"
    
    ! Setup: Create strings with different first character
    str1(1:5) = (/ 'H', 'E', 'L', 'L', 'O' /)
    str2(1:5) = (/ 'X', 'E', 'L', 'L', 'O' /)
    length = 5
    
    ! Call W3AI24
    result = W3AI24(str1, str2, length)
    
    ! Verify: Should return .FALSE.
    IF (result) THEN
      WRITE(*,*) "  FAILED: Expected FALSE for mismatching first char, got TRUE"
      status = 1
      RETURN
    END IF
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_FIRST_CHAR_MISMATCH

  ! Test: Last character mismatch - returns .FALSE.
  SUBROUTINE TEST_LAST_CHAR_MISMATCH(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: str1(10), str2(10)
    INTEGER :: length
    LOGICAL :: result
    
    WRITE(*,*) "Test 3: Last character mismatch"
    
    ! Setup: Create strings with different last character
    str1(1:5) = (/ 'H', 'E', 'L', 'L', 'O' /)
    str2(1:5) = (/ 'H', 'E', 'L', 'L', 'X' /)
    length = 5
    
    ! Call W3AI24
    result = W3AI24(str1, str2, length)
    
    ! Verify: Should return .FALSE.
    IF (result) THEN
      WRITE(*,*) "  FAILED: Expected FALSE for mismatching last char, got TRUE"
      status = 1
      RETURN
    END IF
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_LAST_CHAR_MISMATCH

END PROGRAM TEST_W3AI24
