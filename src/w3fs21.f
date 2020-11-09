C> @file
C                .      .    .                                       .
C> SUBPROGRAM:   W3FS21       NUMBER OF MINUTES SINCE JAN 1, 1978
C>   PRGMMR: REJONES          ORG: NMC421     DATE: 89-07-17
C>
C> ABSTRACT: CALCULATES THE NUMBER OF MINUTES SINCE 0000,
C>   1 JANUARY 1978.
C>
C> PROGRAM HISTORY LOG:
C>   84-06-21  A. DESMARAIS
C>   89-07-14  R.E.JONES    CONVERT TO CYBER 205 FORTRAN 200,
C>                          CHANGE LOGIC SO IT WILL WORK IN
C>                          21 CENTURY.
C>   89-11-02  R.E.JONES    CONVERT TO CRAY CFT77 FORTRAN
C>
C> USAGE:    CALL W3FS21 (IDATE, NMIN)
C>   INPUT ARGUMENT LIST:
C>     IDATE    - INTEGER  SIZE 5 ARRAY CONTAINING YEAR OF CENTURY,
C>                MONTH, DAY, HOUR AND MINUTE.  IDATE(1) MAY BE
C>                A TWO DIGIT YEAR OR 4. IF 2 DIGITS AND GE THAN 78
C>                1900 IS ADDED TO IT. IF LT 78 THEN 2000 IS ADDED
C>                TO IT. IF 4 DIGITS THE SUBROUTINE WILL WORK
C>                CORRECTLY TO THE YEAR 3300 A.D.
C>
C>   OUTPUT ARGUMENT LIST:
C>     NMIN     - INTEGER NUMBER OF MINUTES SINCE 1 JANUARY 1978
C>
C>   SUBPROGRAMS CALLED:
C>     LIBRARY:
C>       W3LIB    - IW3JDN
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN
C>   MACHINE:  CRAY Y-MP8/832
C>
      SUBROUTINE W3FS21(IDATE, NMIN)
C
      INTEGER  IDATE(5)
      INTEGER  NMIN
      INTEGER  JDN78
C
      DATA  JDN78 / 2443510 /
C
C***   IDATE(1)       YEAR OF CENTURY
C***   IDATE(2)       MONTH OF YEAR
C***   IDATE(3)       DAY OF MONTH
C***   IDATE(4)       HOUR OF DAY
C***   IDATE(5)       MINUTE OF HOUR
C
      NMIN  = 0
C
      IYEAR = IDATE(1)
C
      IF (IYEAR.LE.99) THEN
        IF (IYEAR.LT.78) THEN
          IYEAR = IYEAR + 2000
        ELSE
          IYEAR = IYEAR + 1900
        ENDIF
      ENDIF
C
C     COMPUTE JULIAN DAY NUMBER FROM YEAR, MONTH, DAY
C
      IJDN  = IW3JDN(IYEAR,IDATE(2),IDATE(3))
C
C     SUBTRACT JULIAN DAY NUMBER OF JAN 1,1978 TO GET THE
C     NUMBER OF DAYS BETWEEN DATES
C
      NDAYS = IJDN - JDN78
C
C***  NUMBER OF MINUTES
C
      NMIN = NDAYS * 1440 + IDATE(4) * 60 + IDATE(5)
C
      RETURN
      END
