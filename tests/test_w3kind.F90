! This program tests the w3kind() subroutine.
! Alex Richert, Apr 2025

program w3kind_test
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    
    ! External subroutine declaration
    external :: w3kind
    
    ! Variables for test results
    integer :: test_kindreal, test_kindint
    integer :: expected_kindreal, expected_kindint
    integer :: num_passed, num_failed
    
    ! Initialize counters
    num_passed = 0
    num_failed = 0
    
    ! Print header
    write(*,*) "Running tests for w3kind subroutine"
    write(*,*) "==================================="
    
    ! Test 1: Basic functionality
    call test_w3kind_basic_functionality()
    
    ! Test 2: Verify consistent with intrinsic functions
    call test_w3kind_consistency_with_intrinsics()
    
    ! Print summary
    write(*,*) 
    write(*,*) "Test Summary:"
    write(*,*) "------------"
    write(*,'(A,I3)') " Passed: ", num_passed
    write(*,'(A,I3)') " Failed: ", num_failed
    
    ! Exit with error code if any tests failed
    if (num_failed > 0) stop 1
    
contains

    ! Test basic functionality of w3kind
    subroutine test_w3kind_basic_functionality()
        ! Call the subroutine to test
        call w3kind(test_kindreal, test_kindint)
        
        ! Verify results are positive integers (kinds should always be positive)
        if (test_kindreal > 0 .and. test_kindint > 0) then
            call register_pass("Basic functionality - kinds are positive")
        else
            call register_fail("Basic functionality - kinds should be positive")
        end if
    end subroutine test_w3kind_basic_functionality
    
    ! Test consistency with intrinsic kind functions
    subroutine test_w3kind_consistency_with_intrinsics()
        ! Calculate expected values directly using intrinsic kind functions
        expected_kindreal = kind(1.0)
        expected_kindint = kind(1)
        
        ! Call the subroutine to test
        call w3kind(test_kindreal, test_kindint)
        
        ! Test real kind
        if (test_kindreal == expected_kindreal) then
            call register_pass("Real kind matches intrinsic kind(1.0)")
        else
            call register_fail("Real kind mismatch - expected " // &
                            trim(int_to_str(expected_kindreal)) // &
                            " but got " // trim(int_to_str(test_kindreal)))
        end if
        
        ! Test integer kind
        if (test_kindint == expected_kindint) then
            call register_pass("Integer kind matches intrinsic kind(1)")
        else
            call register_fail("Integer kind mismatch - expected " // &
                            trim(int_to_str(expected_kindint)) // &
                            " but got " // trim(int_to_str(test_kindint)))
        end if
    end subroutine test_w3kind_consistency_with_intrinsics
    
    ! Helper function to convert integer to string
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=32) :: str
        
        write(str, '(I0)') num
    end function int_to_str
    
    ! Register a passed test
    subroutine register_pass(message)
        character(len=*), intent(in) :: message
        
        num_passed = num_passed + 1
        write(*,'(A,A)') " PASS: ", trim(message)
    end subroutine register_pass
    
    ! Register a failed test
    subroutine register_fail(message)
        character(len=*), intent(in) :: message
        
        num_failed = num_failed + 1
        write(*,'(A,A)') " FAIL: ", trim(message)
    end subroutine register_fail
    
end program w3kind_test
