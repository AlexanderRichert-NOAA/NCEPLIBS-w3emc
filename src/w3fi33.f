C> The purpose of this subroutine is to convert an array of the first 8
C> identification words into an array of 27 data field identifiers.
C> Taken from https://ntrs.nasa.gov/api/citations/19820002778/downloads/19820002778.pdf
C> Usage:
C>   call w3fi33(idnt, larray)
C>
C> @author Alex Richert @date May 2024
C>
C> @param idnt Array containing the 8 identification words
C> @param larray Array to receive the 27 data field identifiers
      subroutine w3fi33 (idnt, larray)
      dimension larray(27),idnt(8),itable(27),j(6),js(6)
C Commented hex values are from original code
C      data j/Z0000000F,Z000000FF,Z00000FFF,Z0000EFFF,Z000FFFFF,
C     /Z00FFFFFF/
      data j /15,255,4096,61439,1048575,16777215/
C      data js/Z00000007,Z0000007F,Z000007FF,Z00007FFF,Z0007FFFF,
C     /Z007FFFFF/
      data js /7,127,2047,32767,524287,8388607/
C      data mask/Z000000FF/
      data mask/255/
C      data itable/Z00140C01,Z00080C01,Z00000801,Z001C0402,Z01081402,
C     1Z01000802,Z001C0403,Z00140803,Z00080C03,Z00000803,Z001C0404,
C     2Z01081404,Z01000804,Z00180805,Z00100805,Z00080805,Z00000805,
C     3Z001C0406,Z00100C06,Z00001006,Z00180807,Z00100807,Z00080807,
C     4Z00000807,Z00180808,Z00100808,Z00001008/
      data itable /1313793,527361,2049,1836034,17306626,16779266,
     11836035,1312771,527363,2051,1836036,17306628,16779268,1574917,
     21050629,526341,2053,1836038,1051654,4102,1574919,1050631,526343,
     32055,1574920,1050632,4104/
      do 50 i=1,26
      isc=itable(i)
      i1=land(isc,mask)
      i2=land(shftr(isc,8),mask)
      i3=land(shftr(isc,16),mask)
      i4=land(shftr(isc,24),mask)
      ix=i2/4
      larray(i)=land(shftr(idnt(i1),i3),j(ix))
      if(i4.eq.0) goto 50
      ib=larray(i)
      ic=shftl(ib,1)
      do 30 m=1,ix
   30 ic=shft(ic,4)
      if(ic.eq.0) goto 50
      larray(i)=-(land(ib,js(ix)))
   50 continue
      return
      end
