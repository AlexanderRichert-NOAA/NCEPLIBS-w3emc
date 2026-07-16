! This is a test in the NCEPLIBS-w3emc project.
!
! Test the getgi() subroutine with a synthetic index file.
!
program test_getgi
  implicit none

  integer, parameter :: reclen = 320
  integer, parameter :: nrec = 2
  integer, parameter :: full_mbuf = reclen * nrec
  integer, parameter :: small_mbuf = reclen + 40
  integer, parameter :: lu = 11

  character(len=1) :: cbuf(full_mbuf)
  integer :: iret, iretba, nlen, nnum

  call write_index_file('test_getgi_valid.idx', .true., .true.)
  call write_index_file('test_getgi_trunc.idx', .true., .false.)
  call write_index_file('test_getgi_bad.idx', .false., .true.)

  print *, 'Testing getgi()...'

  call baopenr(lu, 'test_getgi_valid.idx', iretba)
  if (iretba /= 0) stop 1
  call cbuf_fill(cbuf)
  call getgi(lu, 0, full_mbuf, cbuf, nlen, nnum, iret)
  call baclose(lu, iretba)
  if (iret /= 0) stop 2
  if (iretba /= 0) stop 3
  if (nlen /= reclen) stop 4
  if (nnum /= nrec) stop 5
  if (any(cbuf(1:reclen) /= 'A')) stop 6
  if (any(cbuf(reclen + 1:full_mbuf) /= 'B')) stop 7

  call baopenr(lu, 'test_getgi_valid.idx', iretba)
  if (iretba /= 0) stop 10
  call cbuf_fill(cbuf)
  call getgi(lu, 1, full_mbuf, cbuf, nlen, nnum, iret)
  call baclose(lu, iretba)
  if (iret /= 0) stop 11
  if (iretba /= 0) stop 12
  if (nlen /= reclen) stop 13
  if (nnum /= 1) stop 14
  if (any(cbuf(1:reclen) /= 'B')) stop 15

  call baopenr(lu, 'test_getgi_valid.idx', iretba)
  if (iretba /= 0) stop 20
  call cbuf_fill(cbuf)
  call getgi(lu, 0, small_mbuf, cbuf, nlen, nnum, iret)
  call baclose(lu, iretba)
  if (iret /= 1) stop 21
  if (iretba /= 0) stop 22
  if (nlen /= reclen) stop 23
  if (nnum /= 1) stop 24
  if (any(cbuf(1:reclen) /= 'A')) stop 25

  call baopenr(lu, 'test_getgi_trunc.idx', iretba)
  if (iretba /= 0) stop 30
  call cbuf_fill(cbuf)
  call getgi(lu, 0, full_mbuf, cbuf, nlen, nnum, iret)
  call baclose(lu, iretba)
  if (iret /= 2) stop 31
  if (iretba /= 0) stop 32
  if (nlen /= reclen) stop 33
  if (nnum /= nrec) stop 34

  call baopenr(lu, 'test_getgi_bad.idx', iretba)
  if (iretba /= 0) stop 40
  call cbuf_fill(cbuf)
  call getgi(lu, 0, full_mbuf, cbuf, nlen, nnum, iret)
  call baclose(lu, iretba)
  if (iret /= 3) stop 41
  if (iretba /= 0) stop 42
  if (nlen /= 0) stop 43
  if (nnum /= 0) stop 44

  print *, 'SUCCESS!'

contains

  subroutine cbuf_fill(buf)
    character(len=1), intent(out) :: buf(:)

    buf = char(0)
  end subroutine cbuf_fill

  subroutine write_index_file(filename, valid_header, write_second_record)
    implicit none

    character(len=*), intent(in) :: filename
    logical, intent(in) :: valid_header, write_second_record

    character(len=81) :: head1, head2
    character(len=reclen) :: rec1, rec2
    integer :: luw

    head1 = ' '
    if (valid_header) then
      head1(42:47) = 'GB1IX1'
    else
      head1(42:47) = 'BADIDX'
    end if

    head2 = ' '
    write(head2, '(8X,3I10,2X,A40)') 162, reclen, nrec, 'test_getgi'
    rec1 = repeat('A', reclen)
    rec2 = repeat('B', reclen)

    open(newunit=luw, file=filename, access='stream', form='unformatted', &
         status='replace', action='write')
    write(luw) head1, head2
    write(luw) rec1
    if (write_second_record) then
      write(luw) rec2
    end if
    close(luw)
  end subroutine write_index_file

end program test_getgi
