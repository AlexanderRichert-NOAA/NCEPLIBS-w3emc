! This program tests getgbm().
program test_getgbm
  implicit none

  character*256 cg1
  integer, parameter :: jfmax=1200000
  integer, parameter :: mbuf=256*1024
  character cbuf(mbuf)
  logical*1 lb(jfmax)
  real f(jfmax)
  integer jpds(200), jgds(200)
  integer kpds(200), kgds(200)
  integer iret, iretba, j, k, kf, k_first
  integer lugb, lugi, lcg1, mnum, nlen, nnum

  cg1='data/input/ref_gdaswave.t00z.wcoast.0p16.f000.grib1'
  lcg1=len_trim(cg1)

  lugb=11
  call baopenr(lugb,cg1(1:lcg1),iretba)
  if(iretba.ne.0) stop 1

  ! Case 1: initialize cached index state from grib file and unpack first hit.
  jpds=-1
  jgds=-1
  kpds=0
  kgds=0
  j=-1
  lugi=0
  call getgbm(lugb,lugi,jfmax,j,jpds,jgds,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,lb,f,iret)
  if(iret.ne.0) stop 2
  if(kf.le.0 .or. k.lt.0) stop 3
  k_first=k

  ! Case 2: non-negative j should use previously cached state.
  jpds=-1
  jgds=-1
  kpds=0
  kgds=0
  j=0
  call getgbm(lugb,lugi,jfmax,j,jpds,jgds,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,lb,f,iret)
  if(iret.ne.0) stop 4
  if(k.ne.k_first) stop 5

  ! Case 3: impossible request should return request-not-found.
  jpds=-1
  jgds=-1
  jpds(1)=999
  kpds=0
  kgds=0
  j=-1
  call getgbm(lugb,lugi,jfmax,j,jpds,jgds,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,lb,f,iret)
  if(iret.ne.99) stop 6

  ! Case 4: too-small output arrays should return length error.
  jpds=-1
  jgds=-1
  kpds=0
  kgds=0
  j=-1
  call getgbm(lugb,lugi,1,j,jpds,jgds,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,lb,f,iret)
  if(iret.ne.98) stop 7

  ! Case 5: invalid index unit should trigger index-read error handling.
  jpds=-1
  jgds=-1
  kpds=0
  kgds=0
  j=-1
  lugi=99
  call getgbm(lugb,lugi,jfmax,j,jpds,jgds,mbuf,cbuf,nlen,nnum,mnum, &
       kf,k,kpds,kgds,lb,f,iret)
  if(iret.ne.96) stop 8

end program test_getgbm