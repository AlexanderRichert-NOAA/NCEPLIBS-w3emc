! This program tests getgbemh().
program test_getgbemh
  implicit none
  character*256 cg1
  integer karg(100)
  integer jpds1(200)
  data jpds1/200*-1/

  integer, parameter :: mbuf=256*1024
  character cbuf1(mbuf), cbuf_ref(mbuf)
  integer jgds(200),jens(5)
  integer kpds1(200),kgds1(200),kens1(5)
  integer lcg1,iarg,iret,iretba,k1,kr1,kr1x
  integer lg1,lx1,m1,mb,mnum1,nlen1,nnum1

  cg1='data/input/ref_gdaswave.t00z.wcoast.0p16.f000.grib1'
  lcg1=len_trim(cg1)
  iarg=iarg+1
  lg1=11
  call baopenr(lg1,cg1(1:lcg1),iretba)
  lx1=0

  !  READ GRIB HEADERS
  mb=1
  kr1=-1
  jgds=-1
  jens=-1
  kpds1=0
  kgds1=0
  kens1=0
  call getgbemh(lg1,lx1,kr1,jpds1,jgds,jens, &
       mbuf,cbuf1,nlen1,nnum1,mnum1, &
       k1,m1,kr1x,kpds1,kgds1,kens1,iret)
!! Uncomment to regenerate baseline data:
!  open(51, file='getgbemh', form='unformatted')
!  write(51) cbuf1
  if(iret.ne.0) stop 1
  if(m1.le.0) stop 2
  open(11, file='data/baseline/getgbemh', form='unformatted')
  read(11) cbuf_ref
  if(any(cbuf1.ne.cbuf_ref)) stop 3

end program test_getgbemh
