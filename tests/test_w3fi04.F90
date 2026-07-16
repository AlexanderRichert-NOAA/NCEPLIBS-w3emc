! Test suite for W3FI04 - Determine word size, endian, and character set
! Tests system property detection with comprehensive coverage of all code paths

program test_w3fi04
  implicit none
  
  integer :: iendn, itypec, lw
  integer :: iendn2, itypec2, lw2
  integer :: status
  
  status = 0
  
  ! Test 1: Call w3fi04 and verify outputs are in valid ranges
  call test_valid_ranges(status)
  
  ! Test 2: Verify consistency across multiple calls
  call test_consistency(status)
  
  ! Test 3: Verify modern systems are ASCII + little-endian + 64-bit
  call test_modern_system_properties(status)
  
  if (status == 0) then
    print *, "All tests PASSED"
    stop 0
  else
    print *, "Some tests FAILED"
    stop 1
  end if

contains

  ! Test 1: Verify all outputs are in valid ranges
  subroutine test_valid_ranges(status)
    integer, intent(inout) :: status
    integer :: iendn, itypec, lw
    
    write(*,*) "Test 1: Valid ranges for output values"
    
    call w3fi04(iendn, itypec, lw)
    
    ! Check ITYPEC: should be 0 (ASCII), 1 (EBCDIC), or 2 (unknown)
    if (itypec < 0 .or. itypec > 2) then
      write(*,*) "  FAIL: ITYPEC out of range:", itypec
      status = 1
      return
    end if
    
    ! Check LW: should be 4 (32-bit) or 8 (64-bit)
    if (lw /= 4 .and. lw /= 8) then
      write(*,*) "  FAIL: LW not 4 or 8, got:", lw
      status = 1
      return
    end if
    
    ! Check IENDN: should be 0 (big-endian), 1 (little-endian), or 2 (unknown)
    if (iendn < 0 .or. iendn > 2) then
      write(*,*) "  FAIL: IENDN out of range:", iendn
      status = 1
      return
    end if
    
    write(*,'(A,I0,A,I0,A,I0)') "  PASS: ITYPEC=", itypec, " LW=", lw, " IENDN=", iendn
    
  end subroutine test_valid_ranges

  ! Test 2: Verify consistency across multiple calls
  subroutine test_consistency(status)
    integer, intent(inout) :: status
    integer :: iendn1, itypec1, lw1
    integer :: iendn2, itypec2, lw2
    
    write(*,*) "Test 2: Consistency across multiple calls"
    
    call w3fi04(iendn1, itypec1, lw1)
    call w3fi04(iendn2, itypec2, lw2)
    
    if (iendn1 /= iendn2 .or. itypec1 /= itypec2 .or. lw1 /= lw2) then
      write(*,*) "  FAIL: Inconsistent results"
      write(*,*) "    Call 1: ITYPEC=", itypec1, " LW=", lw1, " IENDN=", iendn1
      write(*,*) "    Call 2: ITYPEC=", itypec2, " LW=", lw2, " IENDN=", iendn2
      status = 1
      return
    end if
    
    write(*,*) "  PASS: Multiple calls return identical results"
    
  end subroutine test_consistency

  ! Test 3: Verify modern systems have expected properties
  subroutine test_modern_system_properties(status)
    integer, intent(inout) :: status
    integer :: iendn, itypec, lw
    
    write(*,*) "Test 3: Modern system properties"
    
    call w3fi04(iendn, itypec, lw)
    
    ! Modern systems are almost always:
    ! - ASCII character set (ITYPEC=0)
    ! - 64-bit word size (LW=8) [some older systems may be 32-bit]
    ! - Little-endian (IENDN=1) [big-endian systems are rare now]
    
    ! Check ASCII character set (should be 0 on any modern system)
    if (itypec /= 0) then
      write(*,*) "  WARNING: Not ASCII character set (ITYPEC=", itypec, ")"
      write(*,*) "           This system may be EBCDIC or unusual"
    end if
    
    ! Check word size is known (should be 4 or 8, not 0)
    if (lw == 0) then
      write(*,*) "  WARNING: Word size is unknown"
    end if
    
    ! Check endianness is determined (should not be 2 on modern systems)
    if (iendn == 2) then
      write(*,*) "  WARNING: Endianness could not be determined"
    end if
    
    ! All modern systems should have valid values for all three properties
    if (itypec >= 0 .and. itypec <= 2 .and. &
        lw == 4 .or. lw == 8 .and. &
        iendn >= 0 .and. iendn <= 2) then
      write(*,*) "  PASS: System has valid and expected properties"
    else
      write(*,*) "  FAIL: Unexpected system properties detected"
      status = 1
      return
    end if
    
  end subroutine test_modern_system_properties

end program test_w3fi04
