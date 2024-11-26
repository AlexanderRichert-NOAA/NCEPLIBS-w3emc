!> @file
!> @brief Return the local date and time
!> @author Mark Iredell @date 1998-01-05

!> This subprogram returns the local date and time
!> in the ncep absolute date and time data structure.
!>
!> @param[in] IDAT (8) NCEP absolute date and time
!> (year, month, day, time zone, hour, minute, second, millisecond)
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3locdat(idat)
      integer idat(8)
      character cdate*8,ctime*10,czone*5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get local date and time but use the character time zone
      call date_and_time(cdate,ctime,czone,idat)
      read(czone,'(i5)') idat(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
