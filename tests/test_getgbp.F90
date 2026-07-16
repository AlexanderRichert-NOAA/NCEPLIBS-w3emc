! This program unit-tests getgbp() control-flow and saved-state behavior.
program test_getgbp
  implicit none

  integer, parameter :: jgmax = 16
  character g(jgmax)
  integer jpds(200), jgds(200), kpds(200), kgds(200)
  integer iret, j, jg, k, kg
  integer lugb, lugi
  integer force_iret, call_count
  integer seen_jj(16)
  common /mock_state/ force_iret, call_count, seen_jj

  force_iret = 0
  call_count = 0
  seen_jj = 0

  jpds = -1
  jgds = -1
  kpds = 0
  kgds = 0
  jg = jgmax

  ! Case 1: LUGI > 0 and LUGI /= cached unit should map J to MIN(J,-1-J).
  lugb = 11
  lugi = 21
  j = 2
  call getgbp(lugb, lugi, jg, j, jpds, jgds, kg, k, kpds, kgds, g, iret)
  if (iret .ne. 0) stop 1
  if (k .ne. -3) stop 2

  ! Case 2: same cached unit with non-negative J should pass J through unchanged.
  j = 2
  call getgbp(lugb, lugi, jg, j, jpds, jgds, kg, k, kpds, kgds, g, iret)
  if (iret .ne. 0) stop 3
  if (k .ne. 2) stop 4

  ! Case 3: LUGI <= 0 path with a new LUGB should also map J to MIN(J,-1-J).
  lugi = 0
  j = 5
  call getgbp(lugb, lugi, jg, j, jpds, jgds, kg, k, kpds, kgds, g, iret)
  if (iret .ne. 0) stop 5
  if (k .ne. -6) stop 6

  ! Case 4: force IRET=96 to exercise cache reset logic in GETGBP.
  force_iret = 96
  lugi = 99
  j = 1
  call getgbp(lugb, lugi, jg, j, jpds, jgds, kg, k, kpds, kgds, g, iret)
  if (iret .ne. 96) stop 7

  ! Case 5: after reset, same unit should reinitialize and remap J again.
  force_iret = 0
  call getgbp(lugb, lugi, jg, j, jpds, jgds, kg, k, kpds, kgds, g, iret)
  if (iret .ne. 0) stop 8
  if (k .ne. -2) stop 9

  if (call_count .ne. 5) stop 10
  if (seen_jj(1) .ne. -3) stop 11
  if (seen_jj(2) .ne. 2) stop 12
  if (seen_jj(3) .ne. -6) stop 13
  if (seen_jj(4) .ne. -2) stop 14
  if (seen_jj(5) .ne. -2) stop 15

end program test_getgbp

subroutine getgbmp(lugb, lugi, jg, j, jpds, jgds, mbuf, cbuf, nlen, nnum, mnum, &
                   kg, k, kpds, kgds, g, iret)
  implicit none
  integer lugb, lugi, jg, j, mbuf, nlen, nnum, mnum
  integer jpds(200), jgds(200), kpds(200), kgds(200)
  integer kg, k, iret
  character cbuf(mbuf), g(jg)
  integer force_iret, call_count
  integer seen_jj(16)
  common /mock_state/ force_iret, call_count, seen_jj

  call_count = call_count + 1
  if (call_count .le. 16) seen_jj(call_count) = j

  k = j
  kg = min(jg, 4)
  if (jg .ge. 1) g(1) = 'G'
  if (jg .ge. 2) g(2) = 'R'
  if (jg .ge. 3) g(3) = 'I'
  if (jg .ge. 4) g(4) = 'B'
  kpds(1) = lugb
  kgds(1) = lugi

  iret = force_iret
end subroutine getgbmp
