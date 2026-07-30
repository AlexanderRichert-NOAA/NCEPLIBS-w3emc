PROGRAM test_w3fi63_us
  IMPLICIT NONE
  
  CHARACTER(1), ALLOCATABLE :: msga(:)
  INTEGER, ALLOCATABLE :: kpds(:), kgds(:), kptr(:)
  LOGICAL*1, ALLOCATABLE :: kbms(:)
  INTEGER :: kret, i
  INTEGER, PARAMETER :: NUM_GRIDS = 80
  INTEGER :: grid_ids(NUM_GRIDS)
  INTEGER :: map_sizes(NUM_GRIDS)
  
  ! Array initialization for grids
  grid_ids = (/ &
       1, 2, 3, 4, 5, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, &
       18, 27, 28, 29, 30, 37, 38, 39, 40, 41, 42, 43, 44, 100, 101, &
       103, 104, 105, 106, 107, 110, 120, 122, 123, 124, 125, 126, 127, 128, 129, &
       130, 132, 138, 139, 140, 145, 146, 147, 148, 150, 151, 160, 161, 163, 170, &
       171, 172, 173, 174, 175, 176, 179, 180, 181, 182, 183, 184, 187, 188, 189, &
       195, 196, 197, 198, 199 /)
       
  map_sizes = (/ &
       1679, 10512, 65160, 259920, 3021, 2385, 5104, 25020, 223920, 99631, 36391, 153811, 74987, 214268, 387136, &
       281866, 4225, 4225, 5365, 5365, 3447, 3447, 3447, 3447, 3447, 3447, 3447, 3447, 6889, 10283, &
       3640, 16170, 6889, 19305, 11040, 103936, 2020800, 162750, 100800, 75360, 102000, 72960, 294912, 663552, 1548800, &
       151987, 385441, 134784, 4160, 32437, 24505, 23572, 69412, 117130, 806010, 205062, 28080, 14111, 727776, 131072, &
       716100, 489900, 9331200, 4147200, 185704, 76845, 977132, 267168, 102860, 64218, 180144, 2953665, 3425565, 563655, 560025, &
       22833, 72225, 739297, 456225, 37249 /)

  ! Provide enough allocation
  ALLOCATE(msga(100))
  ALLOCATE(kpds(200))
  ALLOCATE(kgds(200))
  ALLOCATE(kptr(200))
  ALLOCATE(kbms(10000000))
  
  DO i = 1, NUM_GRIDS
     msga = CHAR(0)
     kpds = 0
     kgds = 0
     kptr = 0
     kbms = .FALSE.
     
     kpds(1) = 7 ! US grid center
     kpds(3) = grid_ids(i)
     kpds(4) = 0 ! No GDS, no BMS flag
     
     kret = 0
     CALL FI634(msga, kptr, kpds, kgds, kbms, kret)
     
     IF (kret /= 0) THEN
        PRINT *, "Test failed for grid ", grid_ids(i), " with kret = ", kret
        STOP 1
     END IF
     
     IF (kptr(10) /= map_sizes(i)) THEN
        PRINT *, "Test failed for grid ", grid_ids(i), " map size mismatch: expected ", map_sizes(i), " got ", kptr(10)
        STOP 1
     END IF
  END DO

  PRINT *, "All US grid tests passed!"
END PROGRAM
