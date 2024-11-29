! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi71() subroutine.
!
! George Gayno
 program test_w3fi71

! Call routine w3fi71 for all defined grids.
! Compare the Grib1 grid description section
! against expected values.

   implicit none
   integer :: igrid, igds(91), ierr

   print*,"Testing w3fi71..."


   print*, "Check grid 1"
   igrid = 1
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 1
   if (.not.all(igds(1:18) .eq. (/0,255,1,73,23,-48090,0,128,48090,0,513669,513669,22500,64,0,0,0,0/))) stop 2

   print*, "Check grid 2"
   igrid = 2
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 3
   if (.not.all(igds(1:18) .eq. (/0,255,0,144,73,90000,0,128,-90000,-2500,2500,2500,0,0,0,0,0,0/))) stop 4

   print*, "Check grid 3"
   igrid = 3
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 5
   if (.not.all(igds(1:18) .eq. (/0,255,0,360,181,90000,0,128,-90000,-1000,1000,1000,0,0,0,0,0,0/))) stop 6

   print*, "Check grid 4"
   igrid = 4
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 7
   if (.not.all(igds(1:18) .eq. (/0,255,0,720,361,90000,0,128,-90000,-500,500,500,0,0,0,0,0,0/))) stop 8

   print*, "Check grid 5"
   igrid = 5
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 9
   if (.not.all(igds(1:18) .eq. (/0,255,5,53,57,7647,-133443,8,-105000,190500,190500,0,64,0,0,0,0,0/))) stop 10

   print*, "Check grid 6"
   igrid = 6
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 11
   if (.not.all(igds(1:18) .eq. (/0,255,5,53,45,7647,-133443,8,-105000,190500,190500,0,64,0,0,0,0,0/))) stop 12

   print*, "Check grid 8"
   igrid = 8
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 13
   if (.not.all(igds(1:18) .eq. (/0,255,1,116,44,-48670,3104,128,61050,0,318830,318830,22500,64,0,0,0,0/))) stop 14

   print*, "Check grid 10"
   igrid = 10
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 15
   if (.not.all(igds(1:18) .eq. (/0,255,0,180,139,64000,1000,128,-74000,359000,1000,2000,0,0,0,0,0,0/))) stop 16

   print*, "Check grid 11"
   igrid = 11
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 17
   if (.not.all(igds(1:18) .eq. (/0,255,0,720,311,77500,0,128,-77500,359500,500,500,0,0,0,0,0,0/))) stop 18

   print*, "Check grid 12"
   igrid = 12
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 19
   if (.not.all(igds(1:18) .eq. (/0,255,0,301,331,55000,260000,128,0,310000,166,166,0,0,0,0,0,0/))) stop 20

   print*, "Check grid 13"
   igrid = 13
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 21
   if (.not.all(igds(1:18) .eq. (/0,255,0,241,151,50000,210000,128,25000,250000,166,166,0,0,0,0,0,0/))) stop 22

   print*, "Check grid 14"
   igrid = 14
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 23
   if (.not.all(igds(1:18) .eq. (/0,255,0,511,301,30000,130000,128,-20000,215000,166,166,0,0,0,0,0,0/))) stop 24

   print*, "Check grid 15"
   igrid = 15
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 25
   if (.not.all(igds(1:18) .eq. (/0,255,0,401,187,75000,140000,128,44000,240000,166,250,0,0,0,0,0,0/))) stop 26

   print*, "Check grid 16"
   igrid = 16
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 27
   if (.not.all(igds(1:18) .eq. (/0,255,0,548,391,74000,165000,128,48000,237933,66,133,0,0,0,0,0,0/))) stop 28

   print*, "Check grid 17"
   igrid = 17
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 29
   if (.not.all(igds(1:18) .eq. (/0,255,0,736,526,50000,195000,128,15000,244000,66,66,0,0,0,0,0,0/))) stop 30

   print*, "Check grid 18"
   igrid = 18
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 31
   if (.not.all(igds(1:18) .eq. (/0,255,0,586,481,47000,261000,128,15000,300000,66,66,0,0,0,0,0,0/))) stop 32

   print*, "Check grid 21"
   igrid = 21
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 33
   if (.not.all(igds(1:55) .eq. (/0,33,0,65535,37,0,0,128,90000,180000,2500,5000,64,0,0,0,0,0,37,37,37,37,37,37,37,37,37,37,37, &
      37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,1/))) stop 34

   print*, "Check grid 22"
   igrid = 22
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 35
   if (.not.all(igds(1:55) .eq. (/0,33,0,65535,37,0,-180000,128,90000,0,2500,5000,64,0,0,0,0,0,37,37,37,37,37,37,37,37,37,37,37, &
      37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,1/))) stop 36

   print*, "Check grid 23"
   igrid = 23
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 37
   if (.not.all(igds(1:55) .eq. (/0,33,0,65535,37,-90000,0,128,0,180000,2500,5000,64,0,0,0,0,0,1,37,37,37,37,37,37,37,37,37,37, &
      37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37/))) stop 38

   print*, "Check grid 24"
   igrid = 24
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 39
   if (.not.all(igds(1:55) .eq. (/0,33,0,65535,37,-90000,-180000,128,0,0,2500,5000,64,0,0,0,0,0,1,37,37,37,37,37,37,37,37,37,37, &
      37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37/))) stop 40

   print*, "Check grid 25"
   igrid = 25
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 41
   if (.not.all(igds(1:37) .eq. (/0,33,0,65535,19,0,0,128,90000,355000,5000,5000,64,0,0,0,0,0,72,72,72,72,72,72,72,72,72,72,72, &
      72,72,72,72,72,72,72,1/))) stop 42

   print*, "Check grid 26"
   igrid = 26
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 43
   if (.not.all(igds(1:37) .eq. (/0,33,0,65535,19,-90000,0,128,0,355000,5000,5000,64,0,0,0,0,0,1,72,72,72,72,72,72,72,72,72,72, &
      72,72,72,72,72,72,72,72/))) stop 44

   print*, "Check grid 27"
   igrid = 27
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 45
   if (.not.all(igds(1:18) .eq. (/0,255,5,65,65,-20826,-125000,8,-80000,381000,381000,0,64,0,0,0,0,0/))) stop 46

   print*, "Check grid 28"
   igrid = 28
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 47
   if (.not.all(igds(1:18) .eq. (/0,255,5,65,65,20826,145000,8,-80000,381000,381000,128,64,0,0,0,0,0/))) stop 48

   print*, "Check grid 29"
   igrid = 29
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 49
   if (.not.all(igds(1:18) .eq. (/0,255,0,145,37,0,0,128,90000,360000,2500,2500,64,0,0,0,0,0/))) stop 50

   print*, "Check grid 30"
   igrid = 30
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 51
   if (.not.all(igds(1:18) .eq. (/0,255,0,145,37,-90000,0,128,0,360000,2500,2500,64,0,0,0,0,0/))) stop 52

   print*, "Check grid 33"
   igrid = 33
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 53
   if (.not.all(igds(1:18) .eq. (/0,255,0,181,46,0,0,128,90000,360000,2000,2000,64,0,0,0,0,0/))) stop 54

   print*, "Check grid 34"
   igrid = 34
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 55
   if (.not.all(igds(1:18) .eq. (/0,255,0,181,46,-90000,0,128,0,360000,2000,2000,64,0,0,0,0,0/))) stop 56

   print*, "Check grid 37"
   igrid = 37
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 57
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,0,-30000,128,90000,60000,1250,65535,64,0,0,0,0,0,73,73,73,73,73,73,73,73,72, &
      72,72,71,71,71,70,70,69,69,68,67,67,66,65,65,64,63,62,61,60,60,59,58,57,56,55,54,52,51,50,49,48,47,45,44,43,42,40,39,38, &
      36,35,33,32,30,29,28,26,25,23,22,20,19,17,16,14,12,11,9,8,6,5,3,2/))) stop 58

   print*, "Check grid 38"
   igrid = 38
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 59
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,0,60000,128,90000,150000,1250,65535,64,0,0,0,0,0,73,73,73,73,73,73,73,73,72, &
      72,72,71,71,71,70,70,69,69,68,67,67,66,65,65,64,63,62,61,60,60,59,58,57,56,55,54,52,51,50,49,48,47,45,44,43,42,40,39,38, &
      36,35,33,32,30,29,28,26,25,23,22,20,19,17,16,14,12,11,9,8,6,5,3,2/))) stop 60

   print*, "Check grid 39"
   igrid = 39
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 61
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,0,150000,128,90000,-120000,1250,65535,64,0,0,0,0,0,73,73,73,73,73,73,73,73, &
      72,72,72,71,71,71,70,70,69,69,68,67,67,66,65,65,64,63,62,61,60,60,59,58,57,56,55,54,52,51,50,49,48,47,45,44,43,42,40,39, &
      38,36,35,33,32,30,29,28,26,25,23,22,20,19,17,16,14,12,11,9,8,6,5,3,2/))) stop 62

   print*, "Check grid 40"
   igrid = 40
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 63
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,0,-120000,128,90000,-30000,1250,65535,64,0,0,0,0,0,73,73,73,73,73,73,73,73,72, &
      72,72,71,71,71,70,70,69,69,68,67,67,66,65,65,64,63,62,61,60,60,59,58,57,56,55,54,52,51,50,49,48,47,45,44,43,42,40,39,38,36, &
      35,33,32,30,29,28,26,25,23,22,20,19,17,16,14,12,11,9,8,6,5,3,2/))) stop 64

   print*, "Check grid 41"
   igrid = 41
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 65
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,-90000,-30000,128,0,60000,1250,65535,64,0,0,0,0,0,2,3,5,6,8,9,11,12,14,16,17, &
      19,20,22,23,25,26,28,29,30,32,33,35,36,38,39,40,42,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,60,60,61,62,63,64,65,65, &
      66,67,67,68,69,69,70,70,71,71,71,72,72,72,73,73,73,73,73,73,73,73/))) stop 66

   print*, "Check grid 42"
   igrid = 42
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 67
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,-90000,60000,128,0,150000,1250,65535,64,0,0,0,0,0,2,3,5,6,8,9,11,12,14,16,17, &
      19,20,22,23,25,26,28,29,30,32,33,35,36,38,39,40,42,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,60,60,61,62,63,64,65,65, &
      66,67,67,68,69,69,70,70,71,71,71,72,72,72,73,73,73,73,73,73,73,73/))) stop 68

   print*, "Check grid 43"
   igrid = 43
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 69
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,-90000,150000,128,0,-120000,1250,65535,64,0,0,0,0,0,2,3,5,6,8,9,11,12,14,16, &
      17,19,20,22,23,25,26,28,29,30,32,33,35,36,38,39,40,42,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,60,60,61,62,63,64,65, &
      65,66,67,67,68,69,69,70,70,71,71,71,72,72,72,73,73,73,73,73,73,73,73/))) stop 70

   print*, "Check grid 44"
   igrid = 44
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 71
   if (.not.all(igds(1:91) .eq. (/0,33,0,65535,73,-90000,-120000,128,0,-30000,1250,65535,64,0,0,0,0,0,2,3,5,6,8,9,11,12,14,16, &
      17,19,20,22,23,25,26,28,29,30,32,33,35,36,38,39,40,42,43,44,45,47,48,49,50,51,52,54,55,56,57,58,59,60,60,61,62,63,64,65, &
      65,66,67,67,68,69,69,70,70,71,71,71,72,72,72,73,73,73,73,73,73,73,73/))) stop 72

   print*, "Check grid 45"
   igrid = 45
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 73
   if (.not.all(igds(1:18) .eq. (/0,255,0,288,145,90000,0,128,-90000,-1250,1250,1250,0,0,0,0,0,0/))) stop 74

   print*, "Check grid 53"
   igrid = 53
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 75
   if (.not.all(igds(1:18) .eq. (/0,255,1,117,51,-61050,0,128,61050,0,318830,318830,22500,64,0,0,0,0/))) stop 76

   print*, "Check grid 55"
   igrid = 55
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 77
   if (.not.all(igds(1:18) .eq. (/0,255,5,87,71,-10947,-154289,8,-105000,254000,254000,0,64,0,0,0,0,0/))) stop 78

   print*, "Check grid 56"
   igrid = 56
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 79
   if (.not.all(igds(1:18) .eq. (/0,255,5,87,71,7647,-133443,8,-105000,127000,127000,0,64,0,0,0,0,0/))) stop 80

   print*, "Check grid 61"
   igrid = 61
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 81
   if (.not.all(igds(1:64) .eq. (/0,33,0,65535,46,0,0,128,90000,180000,2000,2000,64,0,0,0,0,0,91,91,91,91,91,91,91,91,91,91,91, &
      91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,1/))) stop 82

   print*, "Check grid 62"
   igrid = 62
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 83
   if (.not.all(igds(1:64) .eq. (/0,33,0,65535,46,0,-180000,128,90000,0,2000,2000,64,0,0,0,0,0,91,91,91,91,91,91,91,91,91,91,91, &
      91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,1/))) stop 84

   print*, "Check grid 63"
   igrid = 63
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 85
   if (.not.all(igds(1:64) .eq. (/0,33,0,65535,46,0,-90000,128,0,180000,2000,2000,64,0,0,0,0,0,1,91,91,91,91,91,91,91,91,91,91, &
      91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91/))) stop 86

   print*, "Check grid 64"
   igrid = 64
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 87
   if (.not.all(igds(1:64) .eq. (/0,33,0,65535,46,-90000,-180000,128,0,0,2000,2000,64,0,0,0,0,0,1,91,91,91,91,91,91,91,91,91,91, &
      91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91/))) stop 88

   print*, "Check grid 83"
   igrid = 83
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 89
   if (.not.all(igds(1:18) .eq. (/0,255,205,758,567,2228,-140481,136,47500,-104000,121,121,64,53492,-10984,0,0,0/))) stop 90

   print*, "Check grid 85"
   igrid = 85
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 91
   if (.not.all(igds(1:18) .eq. (/0,255,0,360,90,500,500,128,89500,359500,1000,1000,64,0,0,0,0,0/))) stop 92

   print*, "Check grid 86"
   igrid = 86
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 93
   if (.not.all(igds(1:18) .eq. (/0,255,0,360,90,-89500,500,128,-500,359500,1000,1000,64,0,0,0,0,0/))) stop 94

   print*, "Check grid 87"
   igrid = 87
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 95
   if (.not.all(igds(1:18) .eq. (/0,255,5,81,62,22876,-120491,8,-105000,68153,68153,0,64,0,0,0,0,0/))) stop 96

   print*, "Check grid 88"
   igrid = 88
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 97
   if (.not.all(igds(1:18) .eq. (/0,255,5,580,548,10000,-128000,8,-105000,15000,15000,0,64,0,0,0,0,0/))) stop 98

   print*, "Check grid 90"
   igrid = 90
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 99
   if (.not.all(igds(1:18) .eq. (/0,255,3,4289,2753,20192,-121554,8,-95000,1270,1270,0,64,0,25000,25000,0,0/))) stop 100

   print*, "Check grid 91"
   igrid = 91
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 101
   if (.not.all(igds(1:18) .eq. (/0,255,5,1649,1105,40530,-178571,8,-150000,2976,2976,0,64,0,0,0,0,0/))) stop 102

   print*, "Check grid 92"
   igrid = 92
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 103
   if (.not.all(igds(1:18) .eq. (/0,255,5,3297,2209,40530,-178571,8,-150000,1488,1488,0,64,0,0,0,0,0/))) stop 104

   print*, "Check grid 93"
   igrid = 93
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 105
   if (.not.all(igds(1:18) .eq. (/0,255,203,223,501,44232,-169996,136,63000,-150000,67,66,64,0,0,0,0,0/))) stop 106

   print*, "Check grid 94"
   igrid = 94
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 107
   if (.not.all(igds(1:18) .eq. (/0,255,205,595,625,34921,-161663,136,54000,-106000,63,54,64,83771,-151721,0,0,0/))) stop 108

   print*, "Check grid 95"
   igrid = 95
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 109
   if (.not.all(igds(1:18) .eq. (/0,255,205,401,325,17609,-76327,136,54000,-106000,31,27,64,18840,-61261,0,0,0/))) stop 110

   print*, "Check grid 96"
   igrid = 96
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 111
   if (.not.all(igds(1:18) .eq. (/0,255,205,373,561,11625,-156339,136,54000,-106000,31,27,64,30429,-157827,0,0,0/))) stop 112

   print*, "Check grid 97"
   igrid = 97
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 113
   if (.not.all(igds(1:18) .eq. (/0,255,205,1371,1100,15947,-125468,136,54000,-106000,42,36,64,45407,-52390,0,0,0/))) stop 114

   print*, "Check grid 98"
   igrid = 98
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 115
   if (.not.all(igds(1:18) .eq. (/0,255,4,192,94,88542,0,128,-88542,-1875,47,1875,0,0,0,0,0,0/))) stop 116

   print*, "Check grid 99"
   igrid = 99
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 117
   if (.not.all(igds(1:18) .eq. (/0,255,203,669,1165,-7450,-144140,136,54000,-106000,90,77,64,0,0,0,0,0/))) stop 118

   print*, "Check grid 100"
   igrid = 100
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 119
   if (.not.all(igds(1:18) .eq. (/0,255,5,83,83,17108,-129296,8,-105000,91452,91452,0,64,0,0,0,0,0/))) stop 120

   print*, "Check grid 101"
   igrid = 101
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 121
   if (.not.all(igds(1:18) .eq. (/0,255,5,113,91,10528,-137146,8,-105000,91452,91452,0,64,0,0,0,0,0/))) stop 122

   print*, "Check grid 103"
   igrid = 103
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 123
   if (.not.all(igds(1:18) .eq. (/0,255,5,65,56,22405,-121352,8,-105000,91452,91452,0,64,0,0,0,0,0/))) stop 124

   print*, "Check grid 104"
   igrid = 104
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 125
   if (.not.all(igds(1:18) .eq. (/0,255,5,147,110,-268,-139475,8,-105000,90755,90755,0,64,0,0,0,0,0/))) stop 126

   print*, "Check grid 105"
   igrid = 105
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 127
   if (.not.all(igds(1:18) .eq. (/0,255,5,83,83,17529,-129296,8,-105000,90755,90755,0,64,0,0,0,0,0/))) stop 128

   print*, "Check grid 106"
   igrid = 106
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 129
   if (.not.all(igds(1:18) .eq. (/0,255,5,165,117,17533,-129296,8,-105000,45373,45373,0,64,0,0,0,0,0/))) stop 130

   print*, "Check grid 107"
   igrid = 107
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 131
   if (.not.all(igds(1:18) .eq. (/0,255,5,120,92,23438,-120168,8,-105000,45373,45373,0,64,0,0,0,0,0/))) stop 132

   print*, "Check grid 110"
   igrid = 110
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 133
   if (.not.all(igds(1:18) .eq. (/0,255,0,464,224,25063,-124938,128,52938,-67063,125,125,64,0,0,0,0,0/))) stop 134

   print*, "Check grid 120"
   igrid = 120
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 135
   if (.not.all(igds(1:18) .eq. (/0,255,204,1200,1684,0,0,8,0,0,0,0,64,0,0,0,0,0/))) stop 136

   print*, "Check grid 122"
   igrid = 122
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 137
   if (.not.all(igds(1:18) .eq. (/0,255,204,350,465,0,0,8,0,0,0,0,64,0,0,0,0,0/))) stop 138

   print*, "Check grid 123"
   igrid = 123
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 139
   if (.not.all(igds(1:18) .eq. (/0,255,204,280,360,0,0,8,0,0,0,0,64,0,0,0,0,0/))) stop 140

   print*, "Check grid 124"
   igrid = 124
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 141
   if (.not.all(igds(1:18) .eq. (/0,255,204,240,314,0,0,8,0,0,0,0,64,0,0,0,0,0/))) stop 142

   print*, "Check grid 125"
   igrid = 125
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 143
   if (.not.all(igds(1:18) .eq. (/0,255,204,300,340,0,0,8,0,0,0,0,64,0,0,0,0,0/))) stop 144

   print*, "Check grid 126"
   igrid = 126
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 145
   if (.not.all(igds(1:18) .eq. (/0,255,4,384,190,89277,0,128,-89277,-938,95,938,0,0,0,0,0,0/))) stop 146

   print*, "Check grid 127"
   igrid = 127
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 147
   if (.not.all(igds(1:18) .eq. (/0,255,4,768,384,89642,0,128,-89642,-469,192,469,0,0,0,0,0,0/))) stop 148

   print*, "Check grid 128"
   igrid = 128
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 149
   if (.not.all(igds(1:18) .eq. (/0,255,4,1152,576,89761,0,128,-89761,-313,288,313,0,0,0,0,0,0/))) stop 150

   print*, "Check grid 129"
   igrid = 129
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 151
   if (.not.all(igds(1:18) .eq. (/0,255,4,1760,880,89844,0,128,-89844,-205,440,205,0,0,0,0,0,0/))) stop 152

   print*, "Check grid 130"
   igrid = 130
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 153
   if (.not.all(igds(1:18) .eq. (/0,255,3,451,337,16281,-126138,8,-95000,13545,13545,0,64,0,25000,25000,0,0/))) stop 154

   print*, "Check grid 132"
   igrid = 132
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 155
   if (.not.all(igds(1:18) .eq. (/0,255,3,697,553,1000,-145500,8,-107000,16232,16232,0,64,0,50000,50000,0,0/))) stop 156

   print*, "Check grid 138"
   igrid = 138
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 157
   if (.not.all(igds(1:18) .eq. (/0,255,3,468,288,21017,-123282,8,-97000,12000,12000,0,64,0,33000,45000,0,0/))) stop 158

   print*, "Check grid 139"
   igrid = 139
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 159
   if (.not.all(igds(1:18) .eq. (/0,255,3,80,52,17721,-161973,8,-157500,12000,12000,0,64,0,19000,21000,0,0/))) stop 160

   print*, "Check grid 140"
   igrid = 140
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 161
   if (.not.all(igds(1:18) .eq. (/0,255,3,199,163,53020,-166477,8,-148600,12000,12000,0,64,0,57000,63000,0,0/))) stop 162

   print*, "Check grid 145"
   igrid = 145
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 163
   if (.not.all(igds(1:18) .eq. (/0,255,3,169,145,32174,-90159,8,-79500,12000,12000,0,64,0,36000,46000,0,0/))) stop 164

   print*, "Check grid 146"
   igrid = 146
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 165
   if (.not.all(igds(1:18) .eq. (/0,255,3,166,142,32353,-89994,8,-79500,12000,12000,0,64,0,36000,46000,0,0/))) stop 166

   print*, "Check grid 147"
   igrid = 147
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 167
   if (.not.all(igds(1:18) .eq. (/0,255,3,268,259,24595,-100998,8,-97000,12000,12000,0,64,0,33000,45000,0,0/))) stop 168

   print*, "Check grid 148"
   igrid = 148
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 169
   if (.not.all(igds(1:18) .eq. (/0,255,3,442,265,21821,-120628,8,-97000,12000,12000,0,64,0,33000,45000,0,0/))) stop 170

   print*, "Check grid 150"
   igrid = 150
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 171
   if (.not.all(igds(1:18) .eq. (/0,255,0,401,201,5000,-100000,128,25000,-60000,100,100,64,0,0,0,0,0/))) stop 172

   print*, "Check grid 151"
   igrid = 151
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 173
   if (.not.all(igds(1:18) .eq. (/0,255,5,478,429,-7450,215860,8,-110000,33812,33812,0,64,0,0,0,0,0/))) stop 174

   print*, "Check grid 160"
   igrid = 160
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 175
   if (.not.all(igds(1:18) .eq. (/0,255,5,180,156,19132,-185837,8,-150000,47625,47625,0,64,0,0,0,0,0/))) stop 176

   print*, "Check grid 161"
   igrid = 161
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 177
   if (.not.all(igds(1:18) .eq. (/0,255,0,137,103,50750,271750,72,-250,-19750,500,500,0,0,0,0,0,0/))) stop 178

   print*, "Check grid 163"
   igrid = 163
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 179
   if (.not.all(igds(1:18) .eq. (/0,255,3,1008,722,20600,-118300,8,-95000,5000,5000,0,64,0,38000,38000,0,0/))) stop 180

   print*, "Check grid 170"
   igrid = 170
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 181
   if (.not.all(igds(1:18) .eq. (/0,255,4,512,256,89463,0,128,-89463,-703,128,703,0,0,0,0,0,0/))) stop 182

   print*, "Check grid 171"
   igrid = 171
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 183
   if (.not.all(igds(1:18) .eq. (/0,255,5,770,930,25032,-119560,0,-80000,12700,12700,0,64,0,0,0,0,0/))) stop 184

   print*, "Check grid 172"
   igrid = 172
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 185
   if (.not.all(igds(1:18) .eq. (/0,255,5,690,710,-36899,-220194,0,-80000,12700,12700,128,64,0,0,0,0,0/))) stop 186

   print*, "Check grid 173"
   igrid = 173
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 187
   if (.not.all(igds(1:18) .eq. (/0,255,0,4320,2160,89958,42,128,-89958,359958,83,83,0,0,0,0,0,0/))) stop 188

   print*, "Check grid 174"
   igrid = 174
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 189
   if (.not.all(igds(1:18) .eq. (/0,255,0,2880,1440,89938,62,128,-89938,-62,125,125,64,0,0,0,0,0/))) stop 190

   print*, "Check grid 175"
   igrid = 175
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 191
   if (.not.all(igds(1:18) .eq. (/0,255,0,556,334,0,130000,128,30060,180040,90,90,64,0,0,0,0,0/))) stop 192

   print*, "Check grid 176"
   igrid = 176
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 193
   if (.not.all(igds(1:18) .eq. (/0,255,0,327,235,49100,-92200,128,40910,-75900,35,50,0,0,0,0,0,0/))) stop 194

   print*, "Check grid 179"
   igrid = 179
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 195
   if (.not.all(igds(1:18) .eq. (/0,255,5,1196,817,-2500,-142500,8,-100000,12679,12679,0,64,0,0,0,0,0/))) stop 196

   print*, "Check grid 180"
   igrid = 180
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 197
   if (.not.all(igds(1:18) .eq. (/0,255,0,759,352,55054,-127000,128,17146,-45136,108,108,0,0,0,0,0,0/))) stop 198

   print*, "Check grid 181"
   igrid = 181
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 199
   if (.not.all(igds(1:18) .eq. (/0,255,0,370,278,30054,-100000,128,138,-60148,108,108,0,0,0,0,0,0/))) stop 200

   print*, "Check grid 182"
   igrid = 182
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 201
   if (.not.all(igds(1:18) .eq. (/0,255,0,278,231,32973,-170000,128,8133,-140084,108,108,0,0,0,0,0,0/))) stop 202

   print*, "Check grid 183"
   igrid = 183
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 203
   if (.not.all(igds(1:18) .eq. (/0,255,0,648,278,75054,-200000,128,45138,-130124,108,108,0,0,0,0,0,0/))) stop 204

   print*, "Check grid 184"
   igrid = 184
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 205
   if (.not.all(igds(1:18) .eq. (/0,255,3,2145,1377,20192,-121554,8,-95000,2540,2540,0,64,0,25000,25000,0,0/))) stop 206

   print*, "Check grid 187"
   igrid = 187
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 207
   if (.not.all(igds(1:18) .eq. (/0,255,3,2145,1597,20192,-121554,8,-95000,2540,2540,0,64,0,25000,25000,0,0/))) stop 208

   print*, "Check grid 188"
   igrid = 188
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 209
   if (.not.all(igds(1:18) .eq. (/0,255,3,709,795,37979,-125958,8,-95000,2540,2540,0,64,0,25000,25000,0,0/))) stop 210

   print*, "Check grid 189"
   igrid = 189
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 211
   if (.not.all(igds(1:18) .eq. (/0,255,5,655,855,51500,-142500,8,-135000,1448,1448,0,64,0,0,0,0,0/))) stop 212

   print*, "Check grid 190"
   igrid = 190
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 213
   if (.not.all(igds(1:18) .eq. (/0,255,205,954,835,-7491,-144134,136,54000,-106000,126,108,64,44540,14802,0,0,0/))) stop 214

   print*, "Check grid 192"
   igrid = 192
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 215
   if (.not.all(igds(1:18) .eq. (/0,255,203,237,387,-3441,-148799,136,50000,-111000,225,207,64,0,0,0,0,0/))) stop 216

   print*, "Check grid 193"
   igrid = 193
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 217
   if (.not.all(igds(1:18) .eq. (/0,255,0,1440,721,90000,0,128,-90000,-250,250,250,0,0,0,0,0,0/))) stop 218

   print*, "Check grid 194"
   igrid = 194
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 219
   if (.not.all(igds(1:18) .eq. (/0,255,1,544,310,15000,-75500,128,22005,-62509,2500,2500,20000,64,0,0,0,0/))) stop 220

   print*, "Check grid 195"
   igrid = 195
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 221
   if (.not.all(igds(1:18) .eq. (/0,255,1,177,129,16829,-68196,128,19747,-63972,2500,2500,20000,64,0,0,0,0/))) stop 222

   print*, "Check grid 196"
   igrid = 196
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 223
   if (.not.all(igds(1:18) .eq. (/0,255,1,321,225,18073,-161525,136,23088,-153869,2500,2500,20000,64,0,0,0,0/))) stop 224

   print*, "Check grid 197"
   igrid = 197
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 225
   if (.not.all(igds(1:18) .eq. (/0,255,3,1073,689,20192,-121550,8,-95000,5079,5079,0,64,0,25000,25000,0,0/))) stop 226

   print*, "Check grid 198"
   igrid = 198
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 227
   if (.not.all(igds(1:18) .eq. (/0,255,5,825,553,40530,-178571,8,-150000,5953,5953,0,64,0,0,0,0,0/))) stop 228

   print*, "Check grid 199"
   igrid = 199
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 229
   if (.not.all(igds(1:18) .eq. (/0,255,1,193,193,12350,-216313,128,16794,-211720,2500,2500,20000,64,0,0,0,0/))) stop 230

   print*, "Check grid 200"
   igrid = 200
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 231
   if (.not.all(igds(1:18) .eq. (/0,255,3,108,94,16201,285720,136,-107000,16232,16232,0,64,0,50000,50000,-90000,0/))) stop 232

   print*, "Check grid 201"
   igrid = 201
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 233
   if (.not.all(igds(1:18) .eq. (/0,255,5,65,65,-20826,-150000,8,-105000,381000,381000,0,64,0,0,0,0,0/))) stop 234

   print*, "Check grid 202"
   igrid = 202
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 235
   if (.not.all(igds(1:18) .eq. (/0,255,5,65,43,7838,-141028,8,-105000,190500,190500,0,64,0,0,0,0,0/))) stop 236

   print*, "Check grid 203"
   igrid = 203
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 237
   if (.not.all(igds(1:18) .eq. (/0,255,5,45,39,19132,-185837,8,-150000,190500,190500,0,64,0,0,0,0,0/))) stop 238

   print*, "Check grid 204"
   igrid = 204
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 239
   if (.not.all(igds(1:18) .eq. (/0,255,1,93,68,-25000,110000,128,60644,-109129,160000,160000,20000,64,0,0,0,0/))) stop 240

   print*, "Check grid 205"
   igrid = 205
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 241
   if (.not.all(igds(1:18) .eq. (/0,255,5,45,39,616,-84904,8,-60000,190500,190500,0,64,0,0,0,0,0/))) stop 242

   print*, "Check grid 206"
   igrid = 206
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 243
   if (.not.all(igds(1:18) .eq. (/0,255,3,51,41,22289,-117991,8,-95000,81271,81271,0,64,0,25000,25000,0,0/))) stop 244

   print*, "Check grid 207"
   igrid = 207
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 245
   if (.not.all(igds(1:18) .eq. (/0,255,5,49,35,42085,-175641,8,-150000,95250,95250,0,64,0,0,0,0,0/))) stop 246

   print*, "Check grid 208"
   igrid = 208
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 247
   if (.not.all(igds(1:18) .eq. (/0,255,1,29,27,9343,-167315,128,28092,-145878,80000,80000,20000,64,0,0,0,0/))) stop 248

   print*, "Check grid 209"
   igrid = 209
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 249
   if (.not.all(igds(1:18) .eq. (/0,255,3,275,223,-4850,-151100,8,-111000,44000,44000,0,64,0,45000,45000,0,0/))) stop 250

   print*, "Check grid 210"
   igrid = 210
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 251
   if (.not.all(igds(1:18) .eq. (/0,255,1,25,25,9000,-77000,128,26422,-58625,80000,80000,20000,64,0,0,0,0/))) stop 252

   print*, "Check grid 211"
   igrid = 211
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 253
   if (.not.all(igds(1:18) .eq. (/0,255,3,93,65,12190,-133459,8,-95000,81271,81271,0,64,0,25000,25000,0,0/))) stop 254

   print*, "Check grid 212"
   igrid = 212
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 255
   if (.not.all(igds(1:18) .eq. (/0,255,3,185,129,12190,-133459,136,-95000,40635,40635,0,64,0,25000,25000,-90000,0/))) stop 256

   print*, "Check grid 213"
   igrid = 213
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 257
   if (.not.all(igds(1:18) .eq. (/0,255,5,129,85,7838,-141028,8,-105000,95250,95250,0,64,0,0,0,0,0/))) stop 258

   print*, "Check grid 214"
   igrid = 214
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 259
   if (.not.all(igds(1:18) .eq. (/0,255,5,97,69,42085,-175641,8,-150000,47625,47625,0,64,0,0,0,0,0/))) stop 260

   print*, "Check grid 215"
   igrid = 215
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 261
   if (.not.all(igds(1:18) .eq. (/0,255,3,369,257,12190,-133459,8,-95000,20318,20318,0,64,0,25000,25000,0,0/))) stop 262

   print*, "Check grid 216"
   igrid = 216
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 263
   if (.not.all(igds(1:18) .eq. (/0,255,5,139,107,30000,-173000,136,-135000,45000,45000,0,64,0,0,0,0,0/))) stop 264

   print*, "Check grid 217"
   igrid = 217
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 265
   if (.not.all(igds(1:18) .eq. (/0,255,5,277,213,30000,-173000,8,-135000,22500,22500,0,64,0,0,0,0,0/))) stop 266

   print*, "Check grid 218"
   igrid = 218
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 267
   if (.not.all(igds(1:18) .eq. (/0,255,3,614,428,12190,-133459,8,-95000,12191,12191,0,64,0,25000,25000,0,0/))) stop 268

   print*, "Check grid 219"
   igrid = 219
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 269
   if (.not.all(igds(1:18) .eq. (/0,255,5,385,465,25032,-119560,0,-80000,25400,25400,0,64,0,0,0,0,0/))) stop 270

   print*, "Check grid 220"
   igrid = 220
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 271
   if (.not.all(igds(1:18) .eq. (/0,255,5,345,355,-36899,-220194,0,-80000,25400,25400,128,64,0,0,0,0,0/))) stop 272

   print*, "Check grid 221"
   igrid = 221
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 273
   if (.not.all(igds(1:18) .eq. (/0,255,3,349,277,1000,-145500,8,-107000,32463,32463,0,64,0,50000,50000,0,0/))) stop 274

   print*, "Check grid 222"
   igrid = 222
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 275
   if (.not.all(igds(1:18) .eq. (/0,255,3,138,112,-4850,-151100,8,-111000,88000,88000,0,64,0,45000,45000,0,0/))) stop 276

   print*, "Check grid 223"
   igrid = 223
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 277
   if (.not.all(igds(1:18) .eq. (/0,255,5,129,129,-20826,-150000,8,-105000,190500,190500,0,64,0,0,0,0,0/))) stop 278

   print*, "Check grid 224"
   igrid = 224
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 279
   if (.not.all(igds(1:18) .eq. (/0,255,5,65,65,20826,120000,8,-105000,381000,381000,128,64,0,0,0,0,0/))) stop 280

   print*, "Check grid 225"
   igrid = 225
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 281
   if (.not.all(igds(1:18) .eq. (/0,255,1,185,135,-25000,-250000,128,60640,-109129,80000,80000,20000,64,0,0,0,0/))) stop 282

   print*, "Check grid 226"
   igrid = 226
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 283
   if (.not.all(igds(1:18) .eq. (/0,255,3,737,513,12190,-133459,8,-95000,10159,10159,0,64,0,25000,25000,0,0/))) stop 284

   print*, "Check grid 227"
   igrid = 227
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 285
   if (.not.all(igds(1:18) .eq. (/0,255,3,1473,1025,12190,-133459,8,-95000,5079,5079,0,64,0,25000,25000,0,0/))) stop 286

   print*, "Check grid 228"
   igrid = 228
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 287
   if (.not.all(igds(1:18) .eq. (/0,255,0,144,73,90000,0,128,-90000,-2500,2500,2500,64,0,0,0,0,0/))) stop 288

   print*, "Check grid 229"
   igrid = 229
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 289
   if (.not.all(igds(1:18) .eq. (/0,255,0,360,181,90000,0,128,-90000,-1000,1000,1000,64,0,0,0,0,0/))) stop 290

   print*, "Check grid 230"
   igrid = 230
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 291
   if (.not.all(igds(1:18) .eq. (/0,255,0,720,361,90000,0,128,-90000,-500,500,500,64,0,0,0,0,0/))) stop 292

   print*, "Check grid 231"
   igrid = 231
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 293
   if (.not.all(igds(1:18) .eq. (/0,255,0,720,181,0,0,128,90000,-500,500,500,64,0,0,0,0,0/))) stop 294

   print*, "Check grid 232"
   igrid = 232
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 295
   if (.not.all(igds(1:18) .eq. (/0,255,0,360,91,0,0,128,90000,-1000,1000,1000,64,0,0,0,0,0/))) stop 296

   print*, "Check grid 233"
   igrid = 233
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 297
   if (.not.all(igds(1:18) .eq. (/0,255,0,288,157,78000,0,128,-78000,-1250,1000,1250,0,0,0,0,0,0/))) stop 298

   print*, "Check grid 234"
   igrid = 234
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 299
   if (.not.all(igds(1:18) .eq. (/0,255,0,133,121,15000,-98000,128,-45000,-65000,250,250,64,0,0,0,0,0/))) stop 300

   print*, "Check grid 235"
   igrid = 235
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 301
   if (.not.all(igds(1:18) .eq. (/0,255,0,720,360,89750,250,128,-89750,-250,500,500,0,0,0,0,0,0/))) stop 302

   print*, "Check grid 236"
   igrid = 236
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 303
   if (.not.all(igds(1:18) .eq. (/0,255,3,151,113,16281,233862,136,-95000,40635,40635,0,64,0,25000,25000,-90000,0/))) stop 304

   print*, "Check grid 237"
   igrid = 237
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 305
   if (.not.all(igds(1:18) .eq. (/0,255,3,54,47,16201,285720,8,-107000,32463,32463,0,64,0,50000,50000,0,0/))) stop 306

   print*, "Check grid 238"
   igrid = 238
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 307
   if (.not.all(igds(1:18) .eq. (/0,255,0,275,203,50250,261750,128,-250,-29750,250,250,0,0,0,0,0,0/))) stop 308

   print*, "Check grid 239"
   igrid = 239
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 309
   if (.not.all(igds(1:18) .eq. (/0,255,0,155,123,75250,159500,128,44750,-123500,250,500,0,0,0,0,0,0/))) stop 310

   print*, "Check grid 240"
   igrid = 240
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 311
   if (.not.all(igds(1:18) .eq. (/0,255,5,1121,881,23098,-119036,8,-105000,4763,4763,0,64,0,0,0,0,0/))) stop 312

   print*, "Check grid 241"
   igrid = 241
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 313
   if (.not.all(igds(1:18) .eq. (/0,255,3,549,445,-4850,-151100,8,-111000,22000,22000,0,64,0,45000,45000,0,0/))) stop 314

   print*, "Check grid 242"
   igrid = 242
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 315
   if (.not.all(igds(1:18) .eq. (/0,255,5,553,425,30000,-173000,8,-135000,11250,11250,0,64,0,0,0,0,0/))) stop 316

   print*, "Check grid 243"
   igrid = 243
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 317
   if (.not.all(igds(1:18) .eq. (/0,255,0,126,101,10000,-170000,128,50000,-120000,400,400,64,0,0,0,0,0/))) stop 318

   print*, "Check grid 244"
   igrid = 244
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 319
   if (.not.all(igds(1:18) .eq. (/0,255,0,275,203,50250,261750,128,-250,-29750,250,250,0,0,0,0,0,0/))) stop 320

   print*, "Check grid 245"
   igrid = 245
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 321
   if (.not.all(igds(1:18) .eq. (/0,255,3,336,372,22980,-92840,8,-80000,8000,8000,0,64,0,35000,35000,0,0/))) stop 322

   print*, "Check grid 246"
   igrid = 246
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 323
   if (.not.all(igds(1:18) .eq. (/0,255,3,332,371,25970,-127973,8,-115000,8000,8000,0,64,0,40000,40000,0,0/))) stop 324

   print*, "Check grid 247"
   igrid = 247
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 325
   if (.not.all(igds(1:18) .eq. (/0,255,3,336,372,22980,-110840,8,-98000,8000,8000,0,64,0,35000,35000,0,0/))) stop 326

   print*, "Check grid 248"
   igrid = 248
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 327
   if (.not.all(igds(1:18) .eq. (/0,255,0,135,101,14500,-71500,128,22000,-61450,75,75,64,0,0,0,0,0/))) stop 328

   print*, "Check grid 249"
   igrid = 249
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 329
   if (.not.all(igds(1:18) .eq. (/0,255,5,367,343,45400,-171600,8,-150000,9868,9868,0,64,0,0,0,0,0/))) stop 330

   print*, "Check grid 250"
   igrid = 250
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 331
   if (.not.all(igds(1:18) .eq. (/0,255,0,135,101,16500,-162000,128,24000,-151950,75,75,64,0,0,0,0,0/))) stop 332

   print*, "Check grid 251"
   igrid = 251
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 333
   if (.not.all(igds(1:18) .eq. (/0,255,0,332,210,26350,-83050,128,47250,-49950,100,100,64,0,0,0,0,0/))) stop 334

   print*, "Check grid 252"
   igrid = 252
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 335
   if (.not.all(igds(1:18) .eq. (/0,255,3,301,225,16281,233862,8,265000,20318,20318,0,64,0,25000,25000,0,0/))) stop 336

   print*, "Check grid 253"
   igrid = 253
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 337
   if (.not.all(igds(1:18) .eq. (/0,255,0,373,224,60500,189750,128,4750,-77250,250,250,0,0,0,0,0,0/))) stop 338

   print*, "Check grid 254"
   igrid = 254
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 339
   if (.not.all(igds(1:18) .eq. (/0,255,1,369,300,-35000,-250000,128,60789,-109129,40000,40000,20000,64,0,0,0,0/))) stop 340

   print*,"SUCCESS"
 end program test_w3fi71
