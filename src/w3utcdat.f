!> @file
!> @brief Return the utc date and time
!> @author Mark Iredell @date 1998-01-05

!> This subprogram returns the utc (greenwich) date and time in the NCEP
!> absolute date and time data structure.
!>
!> @param[in] IDAT NCEP absolute date and time (year, month, day, time zone,
!> hour, minute, second, millisecond)
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3utcdat(idat)
      integer idat(8)
      character cdate*8,ctime*10,czone*5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get local date and time but use the character time zone
      call date_and_time(cdate,ctime,czone,idat)
      read(czone,'(i5)') idat(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  convert to hours and minutes to UTC time
!  and possibly adjust the date as well
      idat(6)=idat(6)-mod(idat(4),100)
      idat(5)=idat(5)-idat(4)/100
      idat(4)=0
      if(idat(6).lt.00) then
        idat(6)=idat(6)+60
        idat(5)=idat(5)-1
      elseif(idat(6).ge.60) then
        idat(6)=idat(6)-60
        idat(5)=idat(5)+1
      endif
      if(idat(5).lt.00) then
        idat(5)=idat(5)+24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))-1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      elseif(idat(5).ge.24) then
        idat(5)=idat(5)-24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))+1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
