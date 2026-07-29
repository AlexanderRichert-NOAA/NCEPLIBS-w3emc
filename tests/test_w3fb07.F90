program test_w3fb07
  implicit none
  real :: xi, xj, alat1, alon1, dx, alonv, alat, alon
  real :: polei, polej, rmll, alo1, ala1
  real :: rerth, pi, ss60, h, dxl, reflon, radpd, rebydx
  integer :: num_fails
  real, parameter :: tol = 1e-3

  num_fails = 0
  rerth = 6.3712E+6
  pi = 3.1416
  ss60 = 1.86603
  radpd = pi / 180.0

  ! Test 1: (1,1) in Northern Hemisphere (YY > 0 or YY < 0 depending on ALO1)
  ! Here ALAT1=40, ALON1=100. ALONV=255.
  ! REFLON = 255 - 270 = -15.
  ! ALO1 = (100 - (-15)) = 115 deg. sin(115) > 0, so YY > 0.
  xi = 1.0
  xj = 1.0
  alat1 = 40.0
  alon1 = 100.0
  dx = 190500.0
  alonv = 255.0
  call w3fb07(xi,xj,alat1,alon1,dx,alonv,alat,alon)
  print *, "Test 1:", alat, alon
  if (abs(alat - alat1) > tol .or. abs(alon - alon1) > tol) then
     print *, "FAILED Test 1"
     num_fails = num_fails + 1
  end if

  ! Test 2: (1,1) in Southern Hemisphere (H = -1)
  ! ALAT1=-40, ALON1=100, ALONV=255.
  ! REFLON = 255 - 90 = 165.
  ! ALO1 = 100 - 165 = -65. sin(-65) < 0.
  ! POLEJ = 1 - (-1) * RMLL * sin(-65) = 1 + RMLL * sin(-65)
  ! YY = (1 - POLEJ)*(-1) = - RMLL * sin(-65) > 0.
  ! So YY > 0 again.
  xi = 1.0
  xj = 1.0
  alat1 = -40.0
  alon1 = 100.0
  dx = -190500.0
  alonv = 255.0
  call w3fb07(xi,xj,alat1,alon1,dx,alonv,alat,alon)
  print *, "Test 2:", alat, alon
  if (abs(alat - alat1) > tol .or. abs(alon - alon1) > tol) then
     print *, "FAILED Test 2"
     num_fails = num_fails + 1
  end if

  ! Test 3: Force YY < 0
  ! We need YY < 0. For NH, YY = RMLL * sin(ALO1) if XI=1, XJ=1.
  ! We need sin(ALO1) < 0.
  ! ALO1 = ALON1 - REFLON = ALON1 - (-15) = ALON1 + 15.
  ! If ALON1 = 200, ALO1 = 215. sin(215) < 0. So YY < 0.
  xi = 1.0
  xj = 1.0
  alat1 = 40.0
  alon1 = 200.0
  dx = 190500.0
  alonv = 255.0
  call w3fb07(xi,xj,alat1,alon1,dx,alonv,alat,alon)
  print *, "Test 3:", alat, alon
  if (abs(alat - alat1) > tol .or. abs(alon - alon1) > tol) then
     print *, "FAILED Test 3"
     num_fails = num_fails + 1
  end if

  ! Test 4: R2 == 0 (Pole)
  h = 1.0
  dxl = 190500.0
  reflon = 255.0 - 270.0
  rebydx = rerth / dxl
  ala1 = alat1 * radpd
  rmll = rebydx * cos(ala1) * ss60 / (1.0 + h * sin(ala1))
  alo1 = (alon1 - reflon) * radpd
  polei = 1.0 - rmll * cos(alo1)
  polej = 1.0 - h * rmll * sin(alo1)

  xi = polei
  xj = polej
  call w3fb07(xi,xj,alat1,alon1,dx,alonv,alat,alon)
  ! At North Pole, alat should be 90.0, alon should be REFLON + 360 (since REFLON=-15 < 0)
  ! Wait, in w3fb07: ALAT = H*90. = 90.0. ALON = REFLON. IF (ALON < 0) ALON = ALON + 360.
  ! So ALON = -15 + 360 = 345.0.
  print *, "Test 4:", alat, alon
  if (abs(alat - 90.0) > tol .or. abs(alon - 345.0) > tol) then
     print *, "FAILED Test 4"
     num_fails = num_fails + 1
  end if
  
  if (num_fails > 0) then
     stop 1
  end if

end program test_w3fb07
