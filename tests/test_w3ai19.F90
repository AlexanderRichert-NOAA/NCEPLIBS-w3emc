! Test suite for W3AI19 - Blocker Subroutine
! Tests all branches including normal operation and error cases
PROGRAM TEST_W3AI19
  IMPLICIT NONE
  
  INTEGER :: status
  
  status = 0
  
  ! Test 1: Initialize block and add first line (NEXT=0)
  CALL TEST_FIRST_LINE(status)
  
  ! Test 2: Add second line to block (NEXT>0)
  CALL TEST_SECOND_LINE(status)
  
  ! Test 3: Line doesn't fit - returns NEXT=-1
  CALL TEST_LINE_OVERFLOW(status)
  
  ! Test 4: Call with NEXT already negative - returns immediately
  CALL TEST_NEXT_NEGATIVE(status)
  
  ! Test 5: Error case - N <= 0 returns NEXT=-2
  CALL TEST_INVALID_N(status)
  
  ! Test 6: Error case - L <= 0 returns NEXT=-3
  CALL TEST_INVALID_L(status)
  
  IF (status == 0) THEN
    PRINT *, "All tests PASSED"
    STOP 0
  ELSE
    PRINT *, "Some tests FAILED"
    STOP 1
  END IF

CONTAINS

  ! Test: First line in block (NEXT=0) - initializes block with blanks
  SUBROUTINE TEST_FIRST_LINE(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: line(10), nblk(20)
    INTEGER :: l, n, next, i
    
    WRITE(*,*) "Test 1: First line (NEXT=0) - block initialization"
    
    ! Setup: Create input line "HELLO"
    line(1:5) = (/ 'H', 'E', 'L', 'L', 'O' /)
    nblk = ' '
    l = 5
    n = 20
    next = 0
    
    ! Call W3AI19
    CALL W3AI19(line, l, nblk, n, next)
    
    ! Verify: NEXT should be updated to L
    IF (next /= 5) THEN
      WRITE(*,*) "  FAILED: Expected next=5, got next=", next
      status = 1
      STOP 1001
    END IF
    
    ! Verify: First 5 chars should contain line data
    DO i = 1, 5
      IF (nblk(i) /= line(i)) THEN
        WRITE(*,*) "  FAILED: Line not copied correctly at position", i
        status = 1
        STOP 1002
      END IF
    END DO
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_FIRST_LINE

  ! Test: Add second line to block (NEXT>0)
  SUBROUTINE TEST_SECOND_LINE(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: line(10), nblk(30)
    INTEGER :: l, n, next, i
    
    WRITE(*,*) "Test 2: Second line (NEXT>0)"
    
    ! Setup: Initialize block with first line at positions 1-5
    nblk = ' '
    nblk(1:5) = (/ 'F', 'I', 'R', 'S', 'T' /)
    line(1:6) = (/ 'S', 'E', 'C', 'O', 'N', 'D' /)
    l = 6
    n = 30
    next = 5  ! Already has 5 chars in block
    
    ! Call W3AI19
    CALL W3AI19(line, l, nblk, n, next)
    
    ! Verify: NEXT should be updated to 5+6=11
    IF (next /= 11) THEN
      WRITE(*,*) "  FAILED: Expected next=11, got next=", next
      status = 1
      STOP 1003
    END IF
    
    ! Verify: Second line should be at positions 6-11
    DO i = 1, 6
      IF (nblk(5+i) /= line(i)) THEN
        WRITE(*,*) "  FAILED: Line not appended correctly at position", 5+i
        status = 1
        STOP 1004
      END IF
    END DO
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_SECOND_LINE

  ! Test: Line doesn't fit in remaining block space
  SUBROUTINE TEST_LINE_OVERFLOW(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: line(10), nblk(20)
    INTEGER :: l, n, next
    
    WRITE(*,*) "Test 3: Line overflow (L+NEXT > N)"
    
    ! Setup: Block size 20, already used 18 chars, try to add 5 more
    nblk = 'X'
    line(1:5) = (/ 'A', 'B', 'C', 'D', 'E' /)
    l = 5
    n = 20
    next = 18
    
    ! Call W3AI19
    CALL W3AI19(line, l, nblk, n, next)
    
    ! Verify: NEXT should be set to -1 (doesn't fit)
    IF (next /= -1) THEN
      WRITE(*,*) "  FAILED: Expected next=-1 (overflow), got next=", next
      status = 1
      STOP 1005
    END IF
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_LINE_OVERFLOW

  ! Test: Call with NEXT already negative (should return immediately)
  SUBROUTINE TEST_NEXT_NEGATIVE(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: line(5), nblk(10)
    INTEGER :: l, n, next
    
    WRITE(*,*) "Test 4: NEXT already negative (early return)"
    
    ! Setup: NEXT=-1, call again
    line = 'A'
    nblk = 'Z'
    l = 3
    n = 10
    next = -1
    
    ! Call W3AI19
    CALL W3AI19(line, l, nblk, n, next)
    
    ! Verify: NEXT should still be -1 (no change)
    IF (next /= -1) THEN
      WRITE(*,*) "  FAILED: Expected next=-1, got next=", next
      status = 1
      STOP 1006
    END IF
    
    ! Verify: nblk should be unchanged (block should be untouched)
    IF (nblk(1) /= 'Z') THEN
      WRITE(*,*) "  FAILED: Block was modified when NEXT < 0"
      status = 1
      STOP 1007
    END IF
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_NEXT_NEGATIVE

  ! Test: Invalid N (N <= 0) should return NEXT=-2
  SUBROUTINE TEST_INVALID_N(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: line(5), nblk(10)
    INTEGER :: l, n, next
    
    WRITE(*,*) "Test 5: Invalid N (N <= 0)"
    
    ! Setup: N=0 (invalid)
    line = 'A'
    nblk = ' '
    l = 5
    n = 0
    next = 0
    
    ! Call W3AI19
    CALL W3AI19(line, l, nblk, n, next)
    
    ! Verify: NEXT should be -2 (error: N invalid)
    IF (next /= -2) THEN
      WRITE(*,*) "  FAILED: Expected next=-2 (invalid N), got next=", next
      status = 1
      STOP 1008
    END IF
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_INVALID_N

  ! Test: Invalid L (L <= 0) should return NEXT=-3
  SUBROUTINE TEST_INVALID_L(status)
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=1) :: line(5), nblk(10)
    INTEGER :: l, n, next
    
    WRITE(*,*) "Test 6: Invalid L (L <= 0)"
    
    ! Setup: L=0 (invalid), but N is valid
    line = 'A'
    nblk = ' '
    l = 0
    n = 10
    next = 0
    
    ! Call W3AI19
    CALL W3AI19(line, l, nblk, n, next)
    
    ! Verify: NEXT should be -3 (error: L invalid)
    IF (next /= -3) THEN
      WRITE(*,*) "  FAILED: Expected next=-3 (invalid L), got next=", next
      status = 1
      STOP 1009
    END IF
    
    WRITE(*,*) "  PASSED"
  END SUBROUTINE TEST_INVALID_L

END PROGRAM TEST_W3AI19
