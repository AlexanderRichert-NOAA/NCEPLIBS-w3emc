! This program tests getgbeh().
program test_getgbeh
  implicit none

  character*256 cg1
  integer jpds(200), jgds(200), jens(200)
  integer kpds(200), kgds(200), kens(200)
  integer iret, iretba, k, kg, kf, k_first
  integer lugb, lugi, j, lcg1

  cg1='data/input/ref_gdaswave.t00z.wcoast.0p16.f000.grib1'
  lcg1=len_trim(cg1)

  lugb=11
  call baopenr(lugb,cg1(1:lcg1),iretba)
  if(iretba.ne.0) stop 1

  ! Case 1: initialize cached state from grib file and find first match.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  j=-1
  lugi=0
  call getgbeh(lugb,lugi,j,jpds,jgds,jens,kg,kf,k,kpds,kgds,kens,iret)
  if(iret.ne.0) stop 2
  if(kg.le.0 .or. kf.le.0 .or. k.lt.0) stop 3
  k_first=k

  ! Case 2: same file/unit with non-negative j should use cached path.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  j=0
  call getgbeh(lugb,lugi,j,jpds,jgds,jens,kg,kf,k,kpds,kgds,kens,iret)
  if(iret.ne.0) stop 4
  if(k.ne.k_first) stop 5

  ! Case 3: impossible search request should return request-not-found.
  jpds=-1
  jgds=-1
  jens=-1
  jpds(1)=999
  kpds=0
  kgds=0
  kens=0
  j=-1
  call getgbeh(lugb,lugi,j,jpds,jgds,jens,kg,kf,k,kpds,kgds,kens,iret)
  if(iret.ne.99) stop 6

  ! Case 4: invalid index logical unit should trigger read-error path.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  lugi=99
  j=-1
  call getgbeh(lugb,lugi,j,jpds,jgds,jens,kg,kf,k,kpds,kgds,kens,iret)
  if(iret.ne.96) stop 7

end program test_getgbeh
