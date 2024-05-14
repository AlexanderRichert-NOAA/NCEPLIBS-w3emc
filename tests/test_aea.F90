program test_aea
  integer, parameter :: nchar=4
  character*1 :: ia(nchar), ie(nchar), ie_ref(nchar)
  integer nc
  nc = -nchar
  ia(1) = 'a'
  ia(2) = 'A'
  ia(3) = 'b'
  ia(4) = 'B'
  call aea(ia, ie, nc)
!! Uncomment to regenerate baseline data:
!  open(12, file='data/baseline/aea',access='direct',recl=nchar)
!  write(12, rec=1) ie
!  close(12)
  open(11, file='data/baseline/aea', access='direct',recl=nchar)
  read(11, rec=1) ie_ref
  close(11)
  if (any(ie.ne.ie_ref)) stop 1
  nc = nchar
  call aea(ia, ie, nc)
  if (ia(1).ne.'a' .or. ia(2).ne.'A' .or. ia(3).ne.'b' .or. ia(4).ne.'B') stop 2
end program
