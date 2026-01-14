! This program tests the xstore() subroutine for several cases.
! Alex Richert, Apr 2025
PROGRAM test_xstore
#ifdef USE_W3EMC_MODULE
#  ifdef KIND_4
#    define W3EMC_MODULE w3emc_4
#  elif defined(KIND_D)
#    define W3EMC_MODULE w3emc_d
#  elif defined(KIND_8)
#    define W3EMC_MODULE w3emc_8
#  endif
  use W3EMC_MODULE, only: xstore
#endif
    IMPLICIT NONE
    
    ! Test case variables
    REAL(8) :: test_array(10)
    REAL(8) :: test_value
    INTEGER :: i, status
    LOGICAL :: test_passed
    
    ! Initialize test status
    status = 0
    
    WRITE(*,*) "Running tests for XSTORE subroutine..."
    
    ! Test 1: Basic functionality - storing a single value
    CALL test_single_value(status)
    
    ! Test 2: Testing with multiple words
    CALL test_multiple_words(status)
    
    ! Test 3: Edge case - one word
    CALL test_one_word(status)
    
    ! Test 4: Testing with different data values
    CALL test_different_values(status)
    
    ! Final status report
    IF (status == 0) THEN
        WRITE(*,*) "All tests PASSED!"
    ELSE
        WRITE(*,*) "Tests FAILED with status:", status
        STOP 1
    END IF
    
CONTAINS

    SUBROUTINE test_single_value(status)
        INTEGER, INTENT(INOUT) :: status
        REAL :: test_array(1)
        REAL :: test_value
        
        WRITE(*,*) "Test 1: Basic functionality - storing a single value"
        
        ! Initialize test data
        test_array(1) = 0.0D0
        test_value = 42.0D0
        
        ! Call the subroutine under test
        CALL XSTORE(test_array, test_value, 1)
        
        ! Verify the results
        IF (test_array(1) /= test_value) THEN
            WRITE(*,*) "  FAILED: Expected", test_value, "but got", test_array(1)
            status = 1
            STOP 101
        ELSE
            WRITE(*,*) "  PASSED"
        END IF
    END SUBROUTINE test_single_value
    
    SUBROUTINE test_multiple_words(status)
        INTEGER, INTENT(INOUT) :: status
        REAL :: test_array(5)
        REAL :: test_value
        INTEGER :: i
        LOGICAL :: test_passed
        
        WRITE(*,*) "Test 2: Testing with multiple words"
        
        ! Initialize test data
        DO i = 1, 5
            test_array(i) = 0.0D0
        END DO
        test_value = 123.456D0
        
        ! Call the subroutine under test
        CALL XSTORE(test_array, test_value, 5)
        
        ! Verify the results
        test_passed = .TRUE.
        DO i = 1, 5
            IF (test_array(i) /= test_value) THEN
                WRITE(*,*) "  FAILED at position", i, ": Expected", test_value, "but got", test_array(i)
                test_passed = .FALSE.
                status = 1
                STOP 102
            END IF
        END DO
        
        IF (test_passed) THEN
            WRITE(*,*) "  PASSED"
        END IF
    END SUBROUTINE test_multiple_words
    
    SUBROUTINE test_one_word(status)
        INTEGER, INTENT(INOUT) :: status
        REAL :: test_array(3)
        REAL :: test_value
        
        WRITE(*,*) "Test 3: Edge case - one word"
        
        ! Initialize test data
        test_array = [1.0D0, 2.0D0, 3.0D0]
        test_value = -99.99D0
        
        ! Call the subroutine under test with MWORDS=1
        CALL XSTORE(test_array, test_value, 1)
        
        ! Verify the results - only first element should be changed
        IF (test_array(1) /= test_value) THEN
            WRITE(*,*) "  FAILED: First element not updated correctly"
            status = 1
            STOP 103
        END IF
        
        IF (test_array(2) /= 2.0D0 .OR. test_array(3) /= 3.0D0) THEN
            WRITE(*,*) "  FAILED: Other elements were incorrectly modified"
            status = 1
            STOP 104
        END IF
        
        WRITE(*,*) "  PASSED"
    END SUBROUTINE test_one_word
    
    SUBROUTINE test_different_values(status)
        INTEGER, INTENT(INOUT) :: status
        REAL :: test_array(4)
        REAL :: values(3)
        INTEGER :: i, j
        LOGICAL :: test_passed
        
        WRITE(*,*) "Test 4: Testing with different data values"
        
        values = [0.0D0, -1.0D7, 3.14159265359D0]
        
        DO j = 1, 3
            ! Initialize test data
            DO i = 1, 4
                test_array(i) = 999.999D0
            END DO
            
            ! Call the subroutine under test
            CALL XSTORE(test_array, values(j), 4)
            
            ! Verify the results
            test_passed = .TRUE.
            DO i = 1, 4
                IF (test_array(i) /= values(j)) THEN
                    WRITE(*,*) "  FAILED with value", values(j), "at position", i
                    test_passed = .FALSE.
                    status = 1
                    STOP 105
                END IF
            END DO
            
            IF (.NOT. test_passed) THEN
                RETURN
            END IF
        END DO
        
        WRITE(*,*) "  PASSED"
    END SUBROUTINE test_different_values
    
END PROGRAM test_xstore
