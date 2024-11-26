C> @file
C> @brief Operational job identifier
C> @author J. Newell @date 1985-10-29

C> Prints identifying information for operational
C> codes. Called at the beginning of a code, w3tagb() prints
C> the program name, the year and julian day of its
C> compilation, and the responsible organization. On a 2nd
C> line it prints the starting date-time. Called at the
C> end of a job, entry routine, w3tage prints a line with the
C> ending date-time and a 2nd line stating the program name
C> and that it has ended.
c>
C> @param[in] PROG Program name character*1
C> @param[in] KYR Year of compilation integer
C> @param[in] JD Julian day of compilation integer
C> @param[in] LF Hundreths of julian day of compilation
C> integer (range is 0 to 99 inclusive)
C> @param[in] ORG Organization code (such as wd42)
C> character*1
C>
C> @remark Full word used in order to have at least
C> seven decimal digits accuracy for value of ddate.
C> subprogram clock and date may differ for each type
C> computer. you may have to change them for another
C> type of computer.
C>
C> @author J. Newell @date 1985-10-29
       SUBROUTINE W3TAGB(PROG,KYR,JD,LF,ORG)
C
         CHARACTER *(*) PROG,ORG
         CHARACTER * 3 JMON(12)
         CHARACTER * 3 DAYW(7)
C
         INTEGER       IDAT(8), JDOW, JDOY, JDAY
C
         SAVE
C
         DATA  DAYW/'SUN','MON','TUE','WEN','THU','FRI','SAT'/
         DATA  JMON  /'JAN','FEB','MAR','APR','MAY','JUN',
     &                'JUL','AUG','SEP','OCT','NOV','DEC'/
C
         CALL START()

         DYR   = KYR
         DYR   = 1.0E+03 * DYR
         DJD   = JD
         DLF   = LF
         DLF   = 1.0E-02 * DLF
         DDATE = DYR + DJD + DLF
         PRINT 600
  600    FORMAT(//,10('* . * . '))
         PRINT 601, PROG, DDATE, ORG
  601    FORMAT(5X,'PROGRAM ',A,' HAS BEGUN. COMPILED ',F10.2,
     &   5X, 'ORG: ',A)
C
         CALL W3LOCDAT(IDAT)
         CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY)
         PRINT 602, JMON(IDAT(2)),IDAT(3),IDAT(1),IDAT(5),IDAT(6),
     &   IDAT(7),IDAT(8),JDOY,DAYW(JDOW),JDAY
  602    FORMAT(5X,'STARTING DATE-TIME  ',A3,1X,I2.2,',',
     &   I4.4,2X,2(I2.2,':'),I2.2,'.',I3.3,2X,I3,2X,A3,2X,I8,//)
         RETURN
C
         ENTRY W3TAGE(PROG)
C
         CALL W3LOCDAT(IDAT)
         CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY)
         PRINT 603, JMON(IDAT(2)),IDAT(3),IDAT(1),IDAT(5),IDAT(6),
     &   IDAT(7),IDAT(8),JDOY,DAYW(JDOW),JDAY
  603    FORMAT(//,5X,'ENDING DATE-TIME    ',A3,1X,I2.2,',',
     &   I4.4,2X,2(I2.2,':'),I2.2,'.',I3.3,2X,I3,2X,A3,2X,I8)
         PRINT 604, PROG
  604    FORMAT(5X,'PROGRAM ',A,' HAS ENDED.')
C 604    FORMAT(5X,'PROGRAM ',A,' HAS ENDED.  CRAY J916/2048')
C 604    FORMAT(5X,'PROGRAM ',A,' HAS ENDED.  CRAY Y-MP EL2/256')
         PRINT 605
  605    FORMAT(10('* . * . '))

         CALL SUMMARY()
C
         RETURN
         END
