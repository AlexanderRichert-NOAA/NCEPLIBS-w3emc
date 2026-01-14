! This program tests the w3movdat() subroutine for several cases.
! Alex Richert, Apr 2025
program test_w3movdat
#ifdef USE_W3EMC_MODULE
#  ifdef KIND_4
#    define W3EMC_MODULE w3emc_4
#  elif defined(KIND_D)
#    define W3EMC_MODULE w3emc_d
#  elif defined(KIND_8)
#    define W3EMC_MODULE w3emc_8
#  endif
  use W3EMC_MODULE, only: w3movdat
#endif
    implicit none
    
    ! Test cases
    call test_basic_addition()
    call test_basic_subtraction()
    call test_zero_interval()
    call test_millisecond_handling()
    call test_day_boundary_crossing()
    call test_month_boundary_crossing()
    call test_year_boundary_crossing()
    call test_leap_year()
    call test_time_zone_preservation()
    call test_large_interval()
    call test_negative_interval()
    
    print *, "All tests passed successfully!"
    
contains

    ! Helper function to check if two arrays are equal
    logical function are_equal_real(arr1, arr2, n, tolerance)
        real, intent(in) :: arr1(:), arr2(:)
        integer, intent(in) :: n
        real, intent(in) :: tolerance
        integer :: i
        
        are_equal_real = .true.
        do i = 1, n
            if (abs(arr1(i) - arr2(i)) > tolerance) then
                are_equal_real = .false.
                return
            end if
        end do
    end function are_equal_real
    
    logical function are_equal_int(arr1, arr2, n)
        integer, intent(in) :: arr1(:), arr2(:)
        integer, intent(in) :: n
        integer :: i
        
        are_equal_int = .true.
        do i = 1, n
            if (arr1(i) /= arr2(i)) then
                are_equal_int = .false.
                return
            end if
        end do
    end function are_equal_int
    
    ! Basic test - Adding a simple time interval (1 day)
    subroutine test_basic_addition()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/1.0, 0.0, 0.0, 0.0, 0.0/)  ! 1 day
        idat = (/2022, 3, 15, 0, 12, 30, 45, 500/)  ! 2022-03-15 12:30:45.500 UTC
        expected = (/2022, 3, 16, 0, 12, 30, 45, 500/)  ! 2022-03-16 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_basic_addition failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 1
        end if
    end subroutine test_basic_addition
    
    ! Basic test - Subtracting a simple time interval (1 day)
    subroutine test_basic_subtraction()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/-1.0, 0.0, 0.0, 0.0, 0.0/)  ! -1 day
        idat = (/2022, 3, 15, 0, 12, 30, 45, 500/)  ! 2022-03-15 12:30:45.500 UTC
        expected = (/2022, 3, 14, 0, 12, 30, 45, 500/)  ! 2022-03-14 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_basic_subtraction failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 2
        end if
    end subroutine test_basic_subtraction
    
    ! Test zero interval (should return the same date)
    subroutine test_zero_interval()
        real :: rinc(5)
        integer :: idat(8), jdat(8)
        
        ! Initialize test data
        rinc = (/0.0, 0.0, 0.0, 0.0, 0.0/)  ! No change
        idat = (/2022, 3, 15, 0, 12, 30, 45, 500/)  ! 2022-03-15 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, idat, 8)) then
            print *, "test_zero_interval failed"
            print *, "Expected: ", idat
            print *, "Got: ", jdat
            STOP 3
        end if
    end subroutine test_zero_interval
    
    ! Test millisecond handling
    subroutine test_millisecond_handling()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/0.0, 0.0, 0.0, 0.0, 750.0/)  ! Add 750 milliseconds
        idat = (/2022, 3, 15, 0, 12, 30, 45, 500/)  ! 2022-03-15 12:30:45.500 UTC
        expected = (/2022, 3, 15, 0, 12, 30, 46, 250/)  ! 2022-03-15 12:30:46.250 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_millisecond_handling failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 4
        end if
    end subroutine test_millisecond_handling
    
    ! Test day boundary crossing
    subroutine test_day_boundary_crossing()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/0.0, 12.0, 0.0, 0.0, 0.0/)  ! Add 12 hours
        idat = (/2022, 3, 15, 0, 18, 30, 45, 500/)  ! 2022-03-15 18:30:45.500 UTC
        expected = (/2022, 3, 16, 0, 6, 30, 45, 500/)  ! 2022-03-16 06:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_day_boundary_crossing failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 5
        end if
    end subroutine test_day_boundary_crossing
    
    ! Test month boundary crossing
    subroutine test_month_boundary_crossing()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/2.0, 0.0, 0.0, 0.0, 0.0/)  ! Add 2 days
        idat = (/2022, 3, 31, 0, 12, 30, 45, 500/)  ! 2022-03-31 12:30:45.500 UTC
        expected = (/2022, 4, 2, 0, 12, 30, 45, 500/)  ! 2022-04-02 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_month_boundary_crossing failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 6
        end if
    end subroutine test_month_boundary_crossing
    
    ! Test year boundary crossing
    subroutine test_year_boundary_crossing()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/5.0, 0.0, 0.0, 0.0, 0.0/)  ! Add 5 days
        idat = (/2022, 12, 29, 0, 12, 30, 45, 500/)  ! 2022-12-29 12:30:45.500 UTC
        expected = (/2023, 1, 3, 0, 12, 30, 45, 500/)  ! 2023-01-03 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_year_boundary_crossing failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 7
        end if
    end subroutine test_year_boundary_crossing
    
    ! Test leap year handling
    subroutine test_leap_year()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/1.0, 0.0, 0.0, 0.0, 0.0/)  ! Add 1 day
        idat = (/2024, 2, 28, 0, 12, 30, 45, 500/)  ! 2024-02-28 12:30:45.500 UTC
        expected = (/2024, 2, 29, 0, 12, 30, 45, 500/)  ! 2024-02-29 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_leap_year failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 8
        end if
        
        ! Test another leap year case
        rinc = (/2.0, 0.0, 0.0, 0.0, 0.0/)  ! Add 2 days
        idat = (/2024, 2, 28, 0, 12, 30, 45, 500/)  ! 2024-02-28 12:30:45.500 UTC
        expected = (/2024, 3, 1, 0, 12, 30, 45, 500/)  ! 2024-03-01 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_leap_year (second case) failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 9
        end if
    end subroutine test_leap_year
    
    ! Test time zone preservation
    subroutine test_time_zone_preservation()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data with non-zero time zone
        rinc = (/1.0, 6.0, 0.0, 0.0, 0.0/)  ! Add 1 day, 6 hours
        idat = (/2022, 3, 15, 5, 12, 30, 45, 500/)  ! 2022-03-15 12:30:45.500 UTC+5
        expected = (/2022, 3, 16, 5, 18, 30, 45, 500/)  ! 2022-03-16 18:30:45.500 UTC+5
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_time_zone_preservation failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 10
        end if
    end subroutine test_time_zone_preservation
    
    ! Test large interval
    subroutine test_large_interval()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/365.0, 0.0, 0.0, 0.0, 0.0/)  ! Add 365 days (approx 1 year)
        idat = (/2022, 3, 15, 0, 12, 30, 45, 500/)  ! 2022-03-15 12:30:45.500 UTC
        expected = (/2023, 3, 15, 0, 12, 30, 45, 500/)  ! 2023-03-15 12:30:45.500 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_large_interval failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 11
        end if
    end subroutine test_large_interval
    
    ! Test negative interval with complex time components
    subroutine test_negative_interval()
        real :: rinc(5)
        integer :: idat(8), jdat(8), expected(8)
        
        ! Initialize test data
        rinc = (/-1.0, -12.0, -30.0, -45.0, -500.0/)  ! Subtract 1 day, 12h, 30m, 45s, 500ms
        idat = (/2022, 3, 15, 0, 12, 30, 45, 500/)  ! 2022-03-15 12:30:45.500 UTC
        expected = (/2022, 3, 14, 0, 0, 0, 0, 0/)  ! 2022-03-14 00:00:00.000 UTC
        
        ! Call the subroutine
        call w3movdat(rinc, idat, jdat)
        
        ! Check result
        if (.not. are_equal_int(jdat, expected, 8)) then
            print *, "test_negative_interval failed"
            print *, "Expected: ", expected
            print *, "Got: ", jdat
            STOP 12
        end if
    end subroutine test_negative_interval
    
end program test_w3movdat
