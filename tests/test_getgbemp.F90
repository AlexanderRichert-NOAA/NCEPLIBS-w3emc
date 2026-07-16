! This program tests getgbemp().
program test_getgbemp
  implicit none

  character*256 cg1
  integer, parameter :: mbuf=256*1024
  integer, parameter :: jgmax=1200000
  character cbuf(mbuf), g(jgmax)
  integer jpds(200), jgds(200), jens(200)
  integer kpds(200), kgds(200), kens(200)
  integer iret, iretba, j, jg, k, kg, k_first
  integer lugb, lugi, lcg1, mnum, nlen, nnum

  cg1='data/input/ref_gdaswave.t00z.wcoast.0p16.f000.grib1'
  lcg1=len_trim(cg1)

  lugb=11
  call baopenr(lugb,cg1(1:lcg1),iretba)
  if(iretba.ne.0) stop 1

  ! Case 1: initialize cached index state from the grib file and read first hit.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  j=-1
  lugi=0
  jg=jgmax
  call getgbemp(lugb,lugi,jg,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kg,k,kpds,kgds,kens,g,iret)
  if(iret.ne.0) stop 2
  if(kg.le.0 .or. k.lt.0) stop 3
  if(g(1).ne.'G' .or. g(2).ne.'R' .or. g(3).ne.'I' .or. g(4).ne.'B') stop 4
  k_first=k

  ! Case 2: non-negative j should reuse previously cached state.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  j=0
  jg=jgmax
  call getgbemp(lugb,lugi,jg,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kg,k,kpds,kgds,kens,g,iret)
  if(iret.ne.0) stop 5
  if(k.ne.k_first) stop 6

  ! Case 3: impossible search request should return request-not-found.
  jpds=-1
  jgds=-1
  jens=-1
  jpds(1)=999
  kpds=0
  kgds=0
  kens=0
  j=-1
  jg=jgmax
  call getgbemp(lugb,lugi,jg,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kg,k,kpds,kgds,kens,g,iret)
  if(iret.ne.99) stop 7

  ! Case 4: too-small output buffer should return the length error code.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  j=-1
  jg=1
  call getgbemp(lugb,lugi,jg,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kg,k,kpds,kgds,kens,g,iret)
  if(iret.ne.98) stop 8

  ! Case 5: invalid index unit should hit index-read error handling.
  jpds=-1
  jgds=-1
  jens=-1
  kpds=0
  kgds=0
  kens=0
  j=-1
  lugi=99
  jg=jgmax
  call getgbemp(lugb,lugi,jg,j,jpds,jgds,jens,mbuf,cbuf,nlen,nnum,mnum, &
       kg,k,kpds,kgds,kens,g,iret)
  if(iret.ne.96) stop 9

end program test_getgbemp