! This program tests getgbh().
program test_getgbh
  implicit none

  character*256 cg1
  integer jpds(200), jgds(200)
  integer kpds(200), kgds(200)
  integer iret, iretba, j, k, kg, kf, k_first
  integer lugb, lugi, lcg1

  cg1='data/input/ref_gdaswave.t00z.wcoast.0p16.f000.grib1'
  lcg1=len_trim(cg1)

  lugb=11
  call baopenr(lugb,cg1(1:lcg1),iretba)
  if(iretba.ne.0) stop 1

  ! Case 1: initialize cached index state from grib file and find first hit.
  jpds=-1
  jgds=-1
  kpds=0
  kgds=0
  j=-1
  lugi=0
  call getgbh(lugb,lugi,j,jpds,jgds,kg,kf,k,kpds,kgds,iret)
  if(iret.ne.0) stop 2
  if(kg.le.0 .or. kf.le.0 .or. k.lt.0) stop 3
  k_first=k

  ! Case 2: non-negative j should use previously cached state.
  jpds=-1
  jgds=-1
  kpds=0
  kgds=0
  j=0
  call getgbh(lugb,lugi,j,jpds,jgds,kg,kf,k,kpds,kgds,iret)
  if(iret.ne.0) stop 4
  if(k.ne.k_first) stop 5

  ! Case 3: impossible request should return request-not-found.
  jpds=-1
  jgds=-1
  jpds(1)=999
  kpds=0
  kgds=0
  j=-1
  call getgbh(lugb,lugi,j,jpds,jgds,kg,kf,k,kpds,kgds,iret)
  if(iret.ne.99) stop 6

  ! Case 4: invalid index unit should trigger index-read error handling.
  jpds=-1
  jgds=-1
  kpds=0
  kgds=0
  j=-1
  lugi=99
  call getgbh(lugb,lugi,j,jpds,jgds,kg,kf,k,kpds,kgds,iret)
  if(iret.ne.96) stop 7

end program test_getgbh