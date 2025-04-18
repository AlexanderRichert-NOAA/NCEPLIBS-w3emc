! Unit test for W3FI63 subroutine that unpacks GRIB fields.
! This program creates its own synthetic data (no file I/O).
! Alex Richert, April 2025
PROGRAM test_w3fi63
  
  IMPLICIT NONE
  
  ! Define arrays for W3FI63 subroutine
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  REAL, ALLOCATABLE :: data_array(:)
  
  ! Define constants for the arrays
  INTEGER, PARAMETER :: PDS_SIZE = 100
  INTEGER, PARAMETER :: GDS_SIZE = 100 
  INTEGER, PARAMETER :: PTR_SIZE = 20
  
  ! Define a simple 4x4 grid (16 points total)
  INTEGER, PARAMETER :: NI = 4   ! Number of points along longitude
  INTEGER, PARAMETER :: NJ = 4   ! Number of points along latitude
  
  ! Define constants for section lengths and positions
  INTEGER :: pds_length, gds_length, bms_length, bds_length
  INTEGER :: is_start, pds_start, gds_start, bms_start, bds_start, es_start
  INTEGER :: grid_size, msg_length, i, j, kret
  
  ! Set grid size
  grid_size = NI * NJ
  
  ! Set fixed section lengths - IMPORTANT: PDS must be exactly 50 bytes
  pds_length = 50       ! PDS length (bytes) - needs to be exactly 50 for the special case
  gds_length = 32       ! GDS length (bytes)
  bms_length = 6        ! BMS length (bytes) - fixed header only, no bitmap for simplicity
  bds_length = 20       ! BDS length (bytes) - simplified for testing
  
  ! Calculate total message length
  msg_length = 8 + pds_length + gds_length + bms_length + bds_length + 4
  
  ! Calculate section start positions
  is_start = 1                          ! Indicator Section start
  pds_start = is_start + 8              ! Product Definition Section start
  gds_start = pds_start + pds_length    ! Grid Description Section start
  bms_start = gds_start + gds_length    ! Bit Map Section start
  bds_start = bms_start + bms_length    ! Binary Data Section start
  es_start = bds_start + bds_length     ! End Section start ("7777")
  
  ! Allocate arrays
  ALLOCATE(kpds(PDS_SIZE))
  ALLOCATE(kgds(GDS_SIZE))
  ALLOCATE(kbms(grid_size))
  ALLOCATE(data_array(grid_size))
  ALLOCATE(kptr(PTR_SIZE))
  ALLOCATE(msga(msg_length))
  
  ! Initialize arrays
  kpds = 0
  kgds = 0
  kbms = .TRUE.
  data_array = 0.0
  kptr = 0
  msga = CHAR(0)
  
  ! *** Section 0: Indicator Section ***
  ! 1. "GRIB" identifier
  msga(is_start:is_start+3) = (/ 'G', 'R', 'I', 'B' /)
  
  ! 2. Total message length (3 bytes)
  msga(is_start+4) = CHAR(0)
  msga(is_start+5) = CHAR(0)
  msga(is_start+6) = CHAR(msg_length)
  
  ! 3. GRIB edition number
  msga(is_start+7) = CHAR(1)  ! GRIB edition 1
  
  ! *** Section 1: Product Definition Section (PDS) ***
  ! PDS length (3 bytes) - MUST BE 50 for the special case
  msga(pds_start) = CHAR(0)
  msga(pds_start+1) = CHAR(0)
  msga(pds_start+2) = CHAR(pds_length)
  
  ! Table Version
  msga(pds_start+3) = CHAR(1)   ! Table Version 1
  
  ! Identification of center
  msga(pds_start+4) = CHAR(7)   ! NCEP
  
  ! Generating process ID
  msga(pds_start+5) = CHAR(2)   ! Process ID 2
  
  ! Grid definition
  msga(pds_start+6) = CHAR(255) ! Grid definition - using GDS (not predefined)
  
  ! GDS/BMS flags (bit 7-1 = GDS present, bit 6-1 = BMS present)
  msga(pds_start+7) = CHAR(192) ! 11000000 binary = GDS and BMS both present
  
  ! Parameter indicator (Table 2)
  msga(pds_start+8) = CHAR(11)  ! Temperature
  
  ! Level type (Table 3)
  msga(pds_start+9) = CHAR(100) ! Pressure level
  
  ! Level value (pressure level in hPa)
  msga(pds_start+10) = CHAR(1)   ! 500 hPa - high byte
  msga(pds_start+11) = CHAR(244) ! 500 hPa - low byte (binary 0x01F4 = 500)
  
  ! Reference time - year
  msga(pds_start+12) = CHAR(23)  ! 2023 (year of century)
  
  ! Reference time - month
  msga(pds_start+13) = CHAR(3)   ! March
  
  ! Reference time - day
  msga(pds_start+14) = CHAR(10)  ! 10th
  
  ! Reference time - hour
  msga(pds_start+15) = CHAR(12)  ! 12Z
  
  ! Reference time - minute
  msga(pds_start+16) = CHAR(0)   ! 0 minutes
  
  ! Forecast time unit
  msga(pds_start+17) = CHAR(1)   ! Hours
  
  ! Period of time (P1)
  msga(pds_start+18) = CHAR(6)   ! 6-hour forecast
  
  ! Period of time (P2)
  msga(pds_start+19) = CHAR(0)   ! Not used for simple forecast
  
  ! Time range indicator
  msga(pds_start+20) = CHAR(0)   ! Forecast product
  
  ! Number included in average (N/A, set to 0)
  msga(pds_start+21) = CHAR(0)   ! High byte
  msga(pds_start+22) = CHAR(0)   ! Low byte
  
  ! Missing from average
  msga(pds_start+23) = CHAR(0)   ! None missing
  
  ! Century of reference time
  msga(pds_start+24) = CHAR(21)  ! 21st century
  
  ! Subcenter
  msga(pds_start+25) = CHAR(0)   ! Reserved
  
  ! Decimal scale factor (D)
  msga(pds_start+26) = CHAR(0)   ! Sign (0 = positive)
  msga(pds_start+27) = CHAR(2)   ! Value (low 15 bits) - set to 2
  
  ! Additional bytes to get to 40 bytes
  msga(pds_start+28:pds_start+31) = CHAR(0)
  
  ! ** SPECIAL 2ND-DIFFERENCE PACKING FIELDS **
  
  ! Bytes 41-44: First value (IBM floating point format)
  ! This simulates a floating point value of approximately 273.15 (freezing point in K)
  ! IBM Floating Point: Sign bit (bit 0), Exponent (bits 1-7), Fraction (bits 8-31)
  msga(pds_start+32) = CHAR(64)      ! 01000000 - positive, exponent 64 (0)
  msga(pds_start+33) = CHAR(17)      ! 00010001 - fraction part
  msga(pds_start+34) = CHAR(17)      ! 00010001 - fraction part
  msga(pds_start+35) = CHAR(85)      ! 01010101 - fraction part
  
  ! Bytes 45-48: First first-difference (IBM floating point format)
  ! This simulates a small difference value of 0.5
  msga(pds_start+36) = CHAR(64)      ! 01000000 - positive, exponent 64 (0)
  msga(pds_start+37) = CHAR(8)       ! 00001000 - fraction part
  msga(pds_start+38) = CHAR(0)       ! 00000000 - fraction part
  msga(pds_start+39) = CHAR(0)       ! 00000000 - fraction part
  
  ! Bytes 49-51: Scale factor (signed 2-byte integer)
  msga(pds_start+40) = CHAR(0)       ! Sign bit (0 = positive)
  msga(pds_start+41) = CHAR(0)       ! High byte of scale factor
  msga(pds_start+42) = CHAR(3)       ! Low byte of scale factor (value = 3)
  
  ! Filling to exactly 50 bytes for PDS
  msga(pds_start+43:pds_start+49) = CHAR(0)
  
  ! *** Section 2: Grid Description Section (GDS) ***
  ! GDS length (3 bytes)
  msga(gds_start) = CHAR(0)
  msga(gds_start+1) = CHAR(0)
  msga(gds_start+2) = CHAR(gds_length)
  
  ! Number of vertical coordinates
  msga(gds_start+3) = CHAR(0)   ! No vertical coordinates
  
  ! PV - octet number (location of list of vertical coordinate parameters)
  msga(gds_start+4) = CHAR(255) ! No vertical coordinates or PL values
  
  ! Data representation type
  msga(gds_start+5) = CHAR(0)   ! Latitude/longitude grid (regular)
  
  ! Number of points along latitude circle (Ni)
  msga(gds_start+6) = CHAR(0)   ! High byte
  msga(gds_start+7) = CHAR(NI)  ! Low byte
  
  ! Number of points along longitude meridian (Nj)
  msga(gds_start+8) = CHAR(0)   ! High byte
  msga(gds_start+9) = CHAR(NJ)  ! Low byte
  
  ! Latitude of first grid point (La1) - 3 bytes - scaled by 1000
  msga(gds_start+10) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+11) = CHAR(0)   ! High byte
  msga(gds_start+12) = CHAR(0)   ! Low byte - 0 degrees N
  
  ! Longitude of first grid point (Lo1) - 3 bytes - scaled by 1000
  msga(gds_start+13) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+14) = CHAR(0)   ! High byte
  msga(gds_start+15) = CHAR(0)   ! Low byte - 0 degrees E
  
  ! Resolution and component flags
  msga(gds_start+16) = CHAR(128) ! Standard resolution
  
  ! Latitude of last grid point (La2) - 3 bytes - scaled by 1000
  msga(gds_start+17) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+18) = CHAR(90)  ! High byte
  msga(gds_start+19) = CHAR(0)   ! Low byte - 90 degrees N
  
  ! Longitude of last grid point (Lo2) - 3 bytes - scaled by 1000
  msga(gds_start+20) = CHAR(0)   ! Sign bit (0 = positive)
  msga(gds_start+21) = CHAR(90)  ! High byte
  msga(gds_start+22) = CHAR(0)   ! Low byte - 90 degrees E
  
  ! Di - longitudinal direction increment (2 bytes) - scaled by 1000
  msga(gds_start+23) = CHAR(0)   ! High byte
  msga(gds_start+24) = CHAR(30)  ! Low byte - 30 degrees
  
  ! Dj - latitudinal direction increment (2 bytes) - scaled by 1000
  msga(gds_start+25) = CHAR(0)   ! High byte
  msga(gds_start+26) = CHAR(30)  ! Low byte - 30 degrees
  
  ! Scanning mode
  msga(gds_start+27) = CHAR(0)   ! Standard scanning mode (bit 7-0)
  
  ! Reserved (4 bytes)
  msga(gds_start+28:gds_start+31) = CHAR(0)  ! Fill with zeros
  
  ! *** Section 3: Bitmap Section (BMS) ***
  ! BMS length (3 bytes)
  msga(bms_start) = CHAR(0)
  msga(bms_start+1) = CHAR(0)
  msga(bms_start+2) = CHAR(bms_length)
  
  ! Unused bits at end of Section 3
  msga(bms_start+3) = CHAR(0)   ! No unused bits
  
  ! Bitmap indicator (0 = bitmap follows)
  msga(bms_start+4) = CHAR(0)
  msga(bms_start+5) = CHAR(0)
  
  ! We're not including an actual bitmap - normally it would go here
  ! This simplifies the test as the system might interpret all grid points as active
  
  ! *** Section 4: Binary Data Section (BDS) ***
  ! BDS length (3 bytes)
  msga(bds_start) = CHAR(0)
  msga(bds_start+1) = CHAR(0)
  msga(bds_start+2) = CHAR(bds_length)
  
  ! Flag (bits 7-4 = 0 simple packing, bits 3-0 = 0 floating point)
  msga(bds_start+3) = CHAR(0)
  
  ! Scale factor (E) (2 bytes)
  msga(bds_start+4) = CHAR(0)   ! Sign (0 = positive)
  msga(bds_start+5) = CHAR(0)   ! Value (15 bits)
  
  ! Reference value (4 bytes - IEEE 32-bit floating point)
  ! We use simple values - this will be interpreted as 0.0
  msga(bds_start+6:bds_start+9) = CHAR(0)
  
  ! Number of bits used for each packed value
  msga(bds_start+10) = CHAR(8)   ! 8 bits per value
  
  ! Some simple packed data (used by our second difference routine)
  DO i = 0, 8
    msga(bds_start+11+i) = CHAR(i)  ! Simple values for testing
  END DO
  
  ! *** Section 5: End Section *** 
  ! "7777" end marker
  msga(es_start:es_start+3) = (/ '7', '7', '7', '7' /)
  
  PRINT *, "Starting unit test for W3FI63 subroutine"
  
  ! Call the W3FI63 subroutine
  CALL W3FI63(msga, kpds, kgds, kbms, data_array, kptr, kret)
  
  ! Check if there was an error with the GRIB message processing
  IF (kret /= 0) THEN
    PRINT *, "Test failed with non-zero kret"
    PRINT *, "Return code (kret):", kret
    STOP 1
  END IF
  
  ! Verify key KPDS values
  IF (kpds(1) /= ICHAR(msga(pds_start+4))) STOP 1  ! Center ID
  IF (kpds(2) /= ICHAR(msga(pds_start+5))) STOP 2  ! Process ID
  IF (kpds(3) /= ICHAR(msga(pds_start+6))) STOP 3  ! Grid ID
  IF (kpds(5) /= ICHAR(msga(pds_start+8))) STOP 5  ! Parameter
  IF (kpds(6) /= ICHAR(msga(pds_start+9))) STOP 6  ! Level type
  IF (kpds(7) /= 500) STOP 7                       ! Level value (should be 500 hPa)
  IF (kpds(8) /= ICHAR(msga(pds_start+12))) STOP 8 ! Year
  IF (kpds(9) /= ICHAR(msga(pds_start+13))) STOP 9 ! Month
  IF (kpds(10) /= ICHAR(msga(pds_start+14))) STOP 10 ! Day
  IF (kpds(11) /= ICHAR(msga(pds_start+15))) STOP 11 ! Hour
  IF (kpds(13) /= ICHAR(msga(pds_start+17))) STOP 13 ! Forecast time unit
  IF (kpds(14) /= ICHAR(msga(pds_start+18))) STOP 14 ! Time range 1
  IF (kpds(21) /= ICHAR(msga(pds_start+24))) STOP 21 ! Century
  IF (kpds(22) /= 2) STOP 22                        ! Decimal scale (should be 2)
  
  ! Verify key KGDS values
  IF (kgds(1) /= ICHAR(msga(gds_start+5))) STOP 101  ! Data representation type
  IF (kgds(2) /= NI) STOP 102                        ! Ni points
  IF (kgds(3) /= NJ) STOP 103                        ! Nj points
  
  ! Verify key KPTR values
  IF (kptr(1) /= msg_length) STOP 201   ! Total message length
  IF (kptr(3) /= pds_length) STOP 203   ! PDS length
  IF (kptr(4) /= gds_length) STOP 204   ! GDS length
  IF (kptr(5) /= bms_length) STOP 205   ! BMS length
  IF (kptr(6) /= bds_length) STOP 206   ! BDS length
  
  PRINT *, "All test_w3fi63 checks passed successfully"
  
  ! Cleanup
  DEALLOCATE(msga, kpds, kgds, kbms, data_array, kptr)
  
END PROGRAM test_w3fi63
