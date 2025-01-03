! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi74() function.
!
! Ed Hartnett, 2/28/23
program test_w3fi74
  implicit none
  integer igrid
  integer igds(200)
  integer icomp
  integer npts
  character*1 gds(200)
  integer lengds
  integer ierr
  integer i
  character expected_gds(32)
  expected_gds(:) = (/ char(0), char(0), char(32), char(0), &
       char(255), char(5), char(2), char(178), char(2), &
       char(198), char(128), char(144), char(35), char(131), &
       char(92), char(34), char(0), char(129), char(56), &
       char(128), char(0), char(49), char(156), char(0), &
       char(49), char(156), char(128), char(64), char(0), &
       char(0), char(0), char(0) /)

  print *, "Testing w3fi74..."
  icomp = 0

  ! Fill the igds array. This call comes from test_w3fi71.F90.
  igrid = 172
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 1

  ! Fill the igds array. This call comes from w3if72.f.
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 2
  if (lengds .ne. 32 .or. npts .ne. 489900) stop 3
  do i = 1, 32
     if (gds(i) .ne. expected_gds(i)) stop 4
     !print *,'char(', ichar(gds(i)), '), '
  end do

  ! Grid 1
  igrid = 1
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 11
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 12
  if (lengds .ne. 42 .or. npts .ne. 1679) stop 13

  igrid = 2
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 21
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 22
  if (lengds .ne. 32 .or. npts .ne. 10512) stop 23

  igrid = 21
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 31
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 32
  if (lengds .ne. 106 .or. npts .ne. 1333) stop 33

  igrid = 22
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 41
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 42
  if (lengds .ne. 106 .or. npts .ne. 1333) stop 43

  igrid = 23
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 51
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 52
  if (lengds .ne. 106 .or. npts .ne. 1333) stop 53

  igrid = 83
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 61
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 62
  if (lengds .ne. 34 .or. npts .ne. 429786) stop 63

  igrid = 90
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 71
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 72
  if (lengds .ne. 42 .or. npts .ne. 11807617) stop 73

  igrid = 93
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 81
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 82
  if (lengds .ne. 32 .or. npts .ne. 111723) stop 83

  igrid = 98
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 91
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 92
  if (lengds .ne. 32 .or. npts .ne. 18048) stop 93

  igrid = 150
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 101
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 102
  if (lengds .ne. 32 .or. npts .ne. 80601) stop 103

  igrid = 190
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 111
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 112
  if (lengds .ne. 34 .or. npts .ne. 796590) stop 113

  igrid = 194
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 121
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 122
  if (lengds .ne. 42 .or. npts .ne. 168640) stop 123

  igrid = 209
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 131
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 132
  if (lengds .ne. 42 .or. npts .ne. 61325) stop 133

  print *, "SUCCESS"
end program test_w3fi74
