! Unit testing framework for FI637 subroutine (grid/size tests by center).
! Alex Richert, Apr 2025

program test_fi637
    implicit none
    
    ! Test counters
    integer :: test_count = 0
    integer :: pass_count = 0
    integer :: fail_count = 0
    
    ! Begin test suite
    call print_header()
    
    ! Category 1: Basic GDS indicator tests
    call test_no_gds_indicated()
    call test_gds_with_invalid_dimensions()
    
    ! Category 2: International grid tests - testing all grid numbers and both branches
    call test_international_grid_branches(21)  ! 21-26 range
    call test_international_grid_branches(24)  ! Another in 21-26 range
    call test_international_grid_37_to_44(38)  ! 37-44 range special test
    call test_international_grid_37_to_44(43)  ! Another in 37-44 range
    call test_international_grid_branches(50)  ! Exactly 50
    call test_international_grid_branches(61)  ! 61-64 range
    call test_international_grid_branches(64)  ! Another in 61-64 range
    
    ! Category 3: ECMWF tests - testing all grid numbers and both branches
    call test_ecmwf_grid_branches(1)   ! 1-16 range
    call test_ecmwf_grid_branches(8)   ! Another in 1-16 range
    call test_ecmwf_grid_branches(16)  ! End of 1-16 range
    call test_ecmwf_us_grid2_special_case()
    call test_ecmwf_invalid_grid(20)   ! Invalid grid
    
    ! Category 4: UK Met Office tests - testing all grid numbers and both branches
    call test_uk_met_grid_branches(25)  ! 25-26 range
    call test_uk_met_grid_branches(26)  ! End of 25-26 range
    call test_uk_met_invalid_grid(30)   ! Invalid grid
    
    ! Category 5: Canada tests
    call test_canada_grids()
    
    ! Category 6: Japan Meteorological Agency tests
    call test_jma_grids()
    
    ! Category 7: Navy-FNOC tests - testing all grid numbers and both branches
    call test_navy_grid_branches(37)  ! 37-44 range
    call test_navy_grid_branches(44)  ! End of 37-44 range
    call test_navy_grid_branches(220) ! 220-221 range
    call test_navy_grid_branches(221) ! End of 220-221 range
    call test_navy_grid_branches(223) ! Exactly 223
    call test_navy_invalid_grid(100)  ! Invalid grid
    
    ! Category 8: US Grid tests - testing representatives from each range
    ! and both branches of IF (I.NE.J)
    call test_us_grid_branches(3)    ! 1-6 range
    call test_us_grid_branches(8)    ! Exactly 8
    call test_us_grid_branches(10)   ! Exactly 10
    call test_us_grid_branches(15)   ! 11-18 range
    call test_us_grid_branches(28)   ! 27-30 range
    call test_us_grid_branches(34)   ! 33-34 range
    call test_us_grid_branches(53)   ! Exactly 53
    call test_us_grid_branches(55)   ! 55-56 range
    call test_us_grid_branches(70)   ! 67-77 range
    call test_us_grid_branches(87)   ! 85-88 range
    call test_us_grid_branches(95)   ! 90-99 range
    call test_us_grid_branches(100)  ! 100-101 range
    call test_us_grid_branches(105)  ! 103-107 range
    call test_us_grid_branches(110)  ! Exactly 110
    call test_us_grid_branches(120)  ! Exactly 120
    call test_us_grid_branches(125)  ! 122-130 range
    call test_us_grid_branches(132)  ! Exactly 132
    call test_us_grid_branches(138)  ! Exactly 138
    call test_us_grid_branches(139)  ! Exactly 139
    call test_us_grid_branches(140)  ! Exactly 140
    call test_us_grid_branches(146)  ! 145-148 range
    call test_us_grid_branches(150)  ! 150-151 range
    call test_us_grid_branches(160)  ! 160-161 range
    call test_us_grid_branches(163)  ! Exactly 163
    call test_us_grid_branches(173)  ! 170-176 range
    call test_us_grid_branches(181)  ! 179-184 range
    call test_us_grid_branches(187)  ! Exactly 187
    call test_us_grid_branches(188)  ! Exactly 188
    call test_us_grid_branches(189)  ! Exactly 189
    call test_us_grid_branches(190)  ! 190, 192 range
    call test_us_grid_branches(195)  ! 193-199 range
    call test_us_grid_branches(230)  ! 200-254 range
    call test_us_invalid_grid(255)   ! Invalid grid
    
    ! Category 9: Unknown center test
    call test_unknown_center()
    
    ! Print summary
    call print_summary()

    if (fail_count.ne.0) stop 1

contains

    ! Helper routine to print test framework header
    subroutine print_header()
        print *, "----------------------------------------"
        print *, "FI637 GRIB Grid/Size Test Suite"
        print *, "Testing subroutine that validates grid sizes"
        print *, "Testing BOTH branches of every IF (I.NE.J) condition"
        print *, "----------------------------------------"
        print *
    end subroutine print_header
    
    ! Helper routine to print test framework summary
    subroutine print_summary()
        print *
        print *, "----------------------------------------"
        print *, "test_fi637 summary:"
        print *, "  Total checks run:    ", test_count
        print *, "  Checks passed:       ", pass_count
        print *, "  Checks failed:       ", fail_count
        print *, "----------------------------------------"
    end subroutine print_summary
    
    ! Helper routine to check test result
    subroutine check_result(test_name, expected, actual)
        character(len=*), intent(in) :: test_name
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        
        test_count = test_count + 1
        
        if (expected == actual) then
            pass_count = pass_count + 1
            print '(A,A,A)', "PASS: ", trim(test_name), ""
        else
            fail_count = fail_count + 1
            print '(A,A,A,I0,A,I0)', "FAIL: ", trim(test_name), &
                  " - Expected: ", expected, ", Got: ", actual
        end if
    end subroutine check_result
    
    !=====================================================================
    ! Category 1: Basic GDS indicator tests
    !=====================================================================
    
    ! Test when GDS is not indicated (bit 7 of KPDS(4) is 0)
    subroutine test_no_gds_indicated()
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: GDS not indicated (bit 7 of KPDS(4) is 0)
        kpds(4) = 0  ! Ensure bit 7 is not set
        j = 1000
        kret = -1  ! Initialize to non-zero to ensure it gets changed
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Check result
        call check_result("No GDS indicated", 0, kret)
    end subroutine test_no_gds_indicated
    
    ! Test when GDS is indicated but KGDS(2) = 65535
    subroutine test_gds_with_invalid_dimensions()
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: GDS indicated but invalid dimensions
        kpds(4) = 128  ! Set bit 7
        kgds(2) = 65535
        j = 1000
        kret = -1  ! Initialize to non-zero to ensure it gets changed
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Check result - when KGDS(2)=65535, it returns with KRET still at its initial value of 0
        call check_result("GDS with invalid dimensions", 0, kret)
    end subroutine test_gds_with_invalid_dimensions
    
    !=====================================================================
    ! Category 2: International grid tests - Both branches of IF (I.NE.J)
    !=====================================================================
    
    ! Test international grid with both matching and mismatching sizes
    subroutine test_international_grid_branches(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Common setup for international grid test
        kpds(4) = 128     ! Set bit 7
        kpds(3) = grid_num ! Specific grid number
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        
        ! TEST BRANCH 1: I == J (matching size)
        j = 5000          ! Expected size (100*50)
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "International grid (", grid_num, ") - I==J branch"
        
        ! Check result - should return KRET=0 when sizes match
        call check_result(test_name, 0, kret)
        
        ! TEST BRANCH 2: I != J (mismatching size)
        j = 4999          ! Mismatch with I=5000
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "International grid (", grid_num, ") - I!=J branch"
        
        ! Check result - should return KRET=1 when sizes don't match
        call check_result(test_name, 1, kret)
    end subroutine test_international_grid_branches
    
    ! Test both branches of the international grid case for grid numbers 37-44
    ! which has a special if-branch in the code
    subroutine test_international_grid_37_to_44(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Common setup for international grid test
        kpds(4) = 128     ! Set bit 7
        
        ! IMPORTANT: Do not set center ID (kpds(1)) to make sure the code enters
        ! the international grid part rather than US or NAVY sections
        kpds(1) = 0       ! Not any of the specific centers
        
        kpds(3) = grid_num ! Specific grid number (37-44 range)
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        
        ! TEST BRANCH 1: I == J (matching size) inside ELSE IF (KPDS(3).GE.37.AND.KPDS(3).LE.44) block
        j = 5000          ! Expected size (100*50)
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "International grid 37-44 (", grid_num, ") - I==J branch"
        
        ! Check result - should return KRET=0 when sizes match
        call check_result(test_name, 0, kret)
        
        ! TEST BRANCH 2: I != J (mismatching size) inside ELSE IF (KPDS(3).GE.37.AND.KPDS(3).LE.44) block
        j = 4999          ! Mismatch with I=5000
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "International grid 37-44 (", grid_num, ") - I!=J branch"
        
        ! Check result - should return KRET=1 when sizes don't match for this branch
        call check_result(test_name, 1, kret)
    end subroutine test_international_grid_37_to_44
    

    
    !=====================================================================
    ! Category 3: ECMWF tests - Both branches of IF (I.NE.J)
    !=====================================================================
    
    ! Test ECMWF grid with both matching and mismatching sizes
    subroutine test_ecmwf_grid_branches(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Common setup for ECMWF grid test
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 98      ! ECMWF center
        kpds(3) = grid_num ! Specific grid number
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        
        ! TEST BRANCH 1: I == J (matching size)
        j = 5000          ! Expected size (100*50)
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "ECMWF grid (", grid_num, ") - I==J branch"
        
        ! Check result - should return KRET=0 when sizes match
        call check_result(test_name, 0, kret)
        
        ! TEST BRANCH 2: I != J (mismatching size)
        j = 4999          ! Mismatch with I=5000
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "ECMWF grid (", grid_num, ") - I!=J branch"
        
        ! Check result - expect kret==9
        call check_result(test_name, 9, kret)
    end subroutine test_ecmwf_grid_branches
    
    ! Test ECMWF US Grid 2 special case - Both branches
    subroutine test_ecmwf_us_grid2_special_case()
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: ECMWF US Grid 2 - with correct size for Grid 2
        kpds(4) = 128    ! Set bit 7
        kpds(1) = 98     ! ECMWF center
        kpds(3) = 2      ! US Grid 2
        kgds(2) = 144    ! Width - This makes I = 10512 (144*73)
        kgds(3) = 73     ! Height
        j = 9999         ! Initial value (different from I)
        kret = -1
        
        ! Call the subroutine - TEST I != J with special 10512 case
        call fi637(j, kpds, kgds, kret)
        
        ! Check results - J should be updated to I (144*73=10512)
        call check_result("ECMWF US Grid 2 special case - I!=J and I=10512", 0, kret)
        call check_result("ECMWF US Grid 2 value update - I!=J and I=10512", 10512, j)
        
        ! Now test with the wrong grid size for Grid 2
        kgds(2) = 143    ! Width (incorrect) - makes I = 10439 (143*73)
        j = 9999         ! Initial value
        kret = -1
        
        ! Call the subroutine - TEST I != J with invalid size
        call fi637(j, kpds, kgds, kret)
        
        ! Check results - should not update J and return KRET=9
        call check_result("ECMWF US Grid 2 with wrong I - I!=J and I!=10512", 9, kret)
        call check_result("ECMWF US Grid 2 value not updated - I!=J and I!=10512", 9999, j)
        
        ! Now test the I == J condition for Grid 2
        kgds(2) = 144    ! Width - correct again
        kgds(3) = 73     ! Height
        j = 10512        ! Exact match for I (144*73)
        kret = -1
        
        ! Call the subroutine - TEST I == J branch
        call fi637(j, kpds, kgds, kret)
        
        ! Check results - should return success with KRET=0
        call check_result("ECMWF US Grid 2 matching size - I==J branch", 0, kret)
        call check_result("ECMWF US Grid 2 value unchanged - I==J branch", 10512, j)
    end subroutine test_ecmwf_us_grid2_special_case
    
    ! Test ECMWF with invalid grid number
    subroutine test_ecmwf_invalid_grid(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: ECMWF with invalid grid number
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 98      ! ECMWF center
        kpds(3) = grid_num ! Invalid grid number (outside 1-16 range)
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        j = 5000          ! Expected size
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "ECMWF with invalid grid number (", grid_num, ")"
        
        ! Check result - should return KRET=5 for invalid grid
        call check_result(test_name, 5, kret)
    end subroutine test_ecmwf_invalid_grid
    
    !=====================================================================
    ! Category 4: UK Met Office tests - Both branches of IF (I.NE.J)
    !=====================================================================
    
    ! Test UK Met Office grid with both matching and mismatching sizes
    subroutine test_uk_met_grid_branches(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Common setup for UK Met grid test
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 74      ! UK Met Office center
        kpds(3) = grid_num ! Specific grid number
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        
        ! TEST BRANCH 1: I == J (matching size)
        j = 5000          ! Expected size (100*50)
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "UK Met grid (", grid_num, ") - I==J branch"
        
        ! Check result - should return KRET=0 when sizes match
        call check_result(test_name, 0, kret)
        
        ! TEST BRANCH 2: I != J (mismatching size)
        j = 4999          ! Mismatch with I=5000
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "UK Met grid (", grid_num, ") - I!=J branch"
        
        ! Check result - should return KRET=1 when sizes don't match
        call check_result(test_name, 1, kret)
    end subroutine test_uk_met_grid_branches
    
    ! Test UK Met Office with invalid grid number
    subroutine test_uk_met_invalid_grid(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: UK Met with invalid grid number
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 74      ! UK Met Office center
        kpds(3) = grid_num ! Invalid grid number (outside 25-26 range)
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        j = 5000          ! Expected size
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "UK Met with invalid grid number (", grid_num, ")"
        
        ! Check result - should return KRET=5 for invalid grid
        call check_result(test_name, 5, kret)
    end subroutine test_uk_met_invalid_grid
    
    !=====================================================================
    ! Category 5: Canada tests
    !=====================================================================
    
    ! Test Canadian grids - always return with KRET unchanged
    subroutine test_canada_grids()
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: Canadian grid
        kpds(4) = 128  ! Set bit 7
        kpds(1) = 54   ! Canadian center
        kpds(3) = 100  ! Any grid number
        kgds(2) = 100  ! Width
        kgds(3) = 50   ! Height
        j = 5000       ! Expected size
        kret = 1       ! Initially set to 1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Check result - should return with KRET unchanged
        call check_result("Canadian grid", 1, kret)
    end subroutine test_canada_grids
    
    !=====================================================================
    ! Category 6: Japan Meteorological Agency tests
    !=====================================================================
    
    ! Test JMA grids - always return with KRET unchanged
    subroutine test_jma_grids()
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: JMA grid
        kpds(4) = 128  ! Set bit 7
        kpds(1) = 34   ! JMA center
        kpds(3) = 100  ! Any grid number
        kgds(2) = 100  ! Width
        kgds(3) = 50   ! Height
        j = 5000       ! Expected size
        kret = 1       ! Initially set to 1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Check result - should return with KRET unchanged
        call check_result("JMA grid", 1, kret)
    end subroutine test_jma_grids
    
    !=====================================================================
    ! Category 7: Navy-FNOC tests - Both branches of IF (I.NE.J)
    !=====================================================================
    
    ! Test Navy-FNOC grid with both matching and mismatching sizes
    subroutine test_navy_grid_branches(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Common setup for Navy grid test
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 58      ! Navy-FNOC center
        kpds(3) = grid_num ! Specific grid number
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        
        ! TEST BRANCH 1: I == J (matching size)
        j = 5000          ! Expected size (100*50)
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "Navy grid (", grid_num, ") - I==J branch"
        
        ! Check result - should return KRET=0 when sizes match
        call check_result(test_name, 0, kret)
        
        ! TEST BRANCH 2: I != J (mismatching size)
        j = 4999          ! Mismatch with I=5000
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "Navy grid (", grid_num, ") - I!=J branch"
        
        ! Check result - should return KRET=1 when sizes don't match
        call check_result(test_name, 1, kret)
    end subroutine test_navy_grid_branches
    
    ! Test Navy-FNOC with invalid grid number
    subroutine test_navy_invalid_grid(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: Navy with invalid grid number
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 58      ! Navy-FNOC center
        kpds(3) = grid_num ! Invalid grid number (outside valid ranges)
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        j = 5000          ! Expected size
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "Navy with invalid grid number (", grid_num, ")"
        
        ! Check result - should return KRET=5 for invalid grid
        call check_result(test_name, 5, kret)
    end subroutine test_navy_invalid_grid
    
    !=====================================================================
    ! Category 8: US Grid tests - Both branches of IF (I.NE.J)
    !=====================================================================
    
    ! Test US grid with both matching and mismatching sizes
    subroutine test_us_grid_branches(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Common setup for US grid test
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 7       ! US center
        kpds(3) = grid_num ! Specific grid number
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        
        ! TEST BRANCH 1: I == J (matching size)
        j = 5000          ! Expected size (100*50)
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "US grid (", grid_num, ") - I==J branch"
        
        ! Check result - should return KRET=0 when sizes match
        call check_result(test_name, 0, kret)
        
        ! TEST BRANCH 2: I != J (mismatching size)
        j = 4999          ! Mismatch with I=5000
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "US grid (", grid_num, ") - I!=J branch"
        
        ! For all US grids, KRET is initially set to 9 in the US section.
        ! When I!=J is detected, code returns with KRET=9
        call check_result(test_name, 9, kret)
    end subroutine test_us_grid_branches
    
    ! Test US with invalid grid number
    subroutine test_us_invalid_grid(grid_num)
        integer, intent(in) :: grid_num
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        character(len=100) :: test_name
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: US with invalid grid number
        kpds(4) = 128     ! Set bit 7
        kpds(1) = 7       ! US center
        kpds(3) = grid_num ! Invalid grid number (outside all valid ranges)
        kgds(2) = 100     ! Width
        kgds(3) = 50      ! Height
        j = 5000          ! Expected size
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Construct test name
        write(test_name, '(A,I0,A)') "US with invalid grid number (", grid_num, ")"
        
        ! Check result - should return KRET=5 for invalid grid
        call check_result(test_name, 5, kret)
    end subroutine test_us_invalid_grid
    
    !=====================================================================
    ! Category 9: Unknown center test
    !=====================================================================
    
    ! Test unknown center
    subroutine test_unknown_center()
        integer :: j, kret
        integer :: kpds(200), kgds(200)
        
        ! Initialize arrays
        kpds = 0
        kgds = 0
        
        ! Set up test condition: Unknown center
        kpds(4) = 128  ! Set bit 7
        kpds(1) = 99   ! Unknown center (not in the list)
        kpds(3) = 10   ! Any grid number
        kgds(2) = 100  ! Width
        kgds(3) = 50   ! Height
        j = 5000       ! Expected size
        kret = -1
        
        ! Call the subroutine
        call fi637(j, kpds, kgds, kret)
        
        ! Check result
        call check_result("Unknown center", 10, kret)
    end subroutine test_unknown_center
    
end program test_fi637
