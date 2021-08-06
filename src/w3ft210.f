C> @file
C
C> SUBROUTINE: W3FT210   CONVERT (361,91) GRID TO (25,25) MERCATOR GRID
C>   AUTHOR:  JONES,R.E.        ORG:  W342         DATE: 93-10-19
C>
C> ABSTRACT:  CONVERT A NORTHERN HEMISPHERE 1.0 DEGREE LAT.,LON. 361 BY
C>   91 GRID TO A REGIONAL - PUERTO RICO (MERCATOR) 25*25 AWIPS 210 
C>   GRID.
C>
C> PROGRAM HISTORY LOG:
C>   93-10-19  R.E.JONES  
C>
C> USAGE:  CALL W3FT210(ALOLA,AMERC,INTERP)
C>
C>   INPUT ARGUMENTS:  ALOLA  - 361*91 GRID 1.0 DEG. LAT,LON GRID N. HEMI.
C>                              32851 POINT GRID. 360 * 181 ONE DEGREE
C>                              GRIB GRID 3 WAS FLIPPED, GREENWISH ADDED
C>                              TO RIGHT SIDE AND CUT TO 361 * 91.  
C>                     INTERP - 1 LINEAR INTERPOLATION , NE.1 BIQUADRATIC
C>
C>   INPUT FILES:  NONE
C>
C>   OUTPUT ARGUMENTS: AMERC - 25*25 GRID OF NORTHERN MERCATOR
C>                             625 POINT GRID IS AWIPS GRID TYPE 210
C>
C>   OUTPUT FILES: ERROR MESSAGE TO FORTRAN OUTPUT FILE
C>
C>   WARNINGS:
C>
C>   1. W1 AND W2 ARE USED TO STORE SETS OF CONSTANTS WHICH ARE
C>   REUSABLE FOR REPEATED CALLS TO THE SUBROUTINE. 20 OTHER ARRAY
C>   ARE SAVED AND REUSED ON THE NEXT CALL.
C>
C>   RETURN CONDITIONS: NORMAL SUBROUTINE EXIT
C>
C>   SUBPROGRAMS CALLED:
C>     UNIQUE :  NONE
C>
C>     LIBRARY:  ASIN , ATAN2
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN
C>   MACHINE:  CRAY C916-128, CRAY Y-MP8/864, CRAY Y-MP EL2/256
C>
      SUBROUTINE W3FT210(ALOLA,AMERC,INTERP)
C
       PARAMETER   (NPTS=625,II=25,JJ=25)
       PARAMETER   (ALATIN=20.000)
       PARAMETER   (PI=3.1416)
       PARAMETER   (DX=80000.0)
       PARAMETER   (ALAT1=9.000)
       PARAMETER   (ALON1=283.000)
C
       REAL        R2(NPTS),      WLON(NPTS)
       REAL        XLAT(NPTS),    XI(II,JJ),   XJ(II,JJ)
       REAL        XII(NPTS),     XJJ(NPTS),   ANGLE(NPTS)
       REAL        ALOLA(361,91), AMERC(NPTS), ERAS(NPTS,4)
       REAL        W1(NPTS),      W2(NPTS)
       REAL        XDELI(NPTS),   XDELJ(NPTS)
       REAL        XI2TM(NPTS),   XJ2TM(NPTS)
C
       INTEGER     IV(NPTS),      JV(NPTS),    JY(NPTS,4)
       INTEGER     IM1(NPTS),     IP1(NPTS),   IP2(NPTS)
C
       LOGICAL     LIN
C
       SAVE
C
       EQUIVALENCE (XI(1,1),XII(1)),(XJ(1,1),XJJ(1))
C
C      DATA  DEGPR /57.2957795/
       DATA  RERTH /6.3712E+6/
       DATA  INTRPO/99/
       DATA  ISWT  /0/
C
      DEGPR  = 180.0 / PI
      RADPD  = PI / 180.0
      CLAIN  = COS(RADPD * ALATIN)
      DELLON = DX / (RERTH * CLAIN)
      DJEO   = (ALOG(TAN(0.5*((ALAT1+90.0)*RADPD))))/DELLON
      LIN = .FALSE.
      IF (INTERP.EQ.1) LIN = .TRUE.
C
      IF  (ISWT.EQ.1)  GO TO  900
C
        DEG    = 1.0
C
C     NEXT 32 LINES OF CODE PUTS SUBROUTINE W3FB09 IN LINE
C
      DO 100 J = 1,JJ
         DO 100 I = 1,II
             XI(I,J) = I
             XJ(I,J) = J
 100     CONTINUE
C
      DO 200 KK = 1,NPTS
        XLAT(KK) = 2.0*ATAN(EXP(DELLON*(DJEO + XJJ(KK)-1.)))
     &              * DEGPR - 90.0
 200  CONTINUE
C
      DO 300 KK = 1,NPTS
        WLON(KK) = (XII(KK) -1.0) * DELLON * DEGPR + ALON1
 300  CONTINUE
C
      DO 400 KK = 1,NPTS
        W1(KK)  = WLON(KK) + 1.0
        W2(KK)  = XLAT(KK) + 1.0
 400  CONTINUE
C
      ISWT   = 1
      INTRPO = INTERP
      GO TO 1000
C
C     AFTER THE 1ST CALL TO W3FT210 TEST INTERP, IF IT HAS
C     CHANGED RECOMPUTE SOME CONSTANTS
C
  900 CONTINUE
        IF (INTERP.EQ.INTRPO) GO TO 2100
        INTRPO = INTERP
C
 1000 CONTINUE
        DO 1100 K = 1,NPTS
          IV(K)    = W1(K)
          JV(K)    = W2(K)
          XDELI(K) = W1(K) - IV(K)
          XDELJ(K) = W2(K) - JV(K)
          IP1(K)   = IV(K) + 1
          JY(K,3)  = JV(K) + 1
          JY(K,2)  = JV(K)
 1100   CONTINUE
C
      IF (.NOT.LIN) THEN
        DO 1200 K = 1,NPTS
          IP2(K)   = IV(K) + 2
          IM1(K)   = IV(K) - 1
          JY(K,1)  = JV(K) - 1
          JY(K,4)  = JV(K) + 2
          XI2TM(K) = XDELI(K) * (XDELI(K) - 1.0) * .25
          XJ2TM(K) = XDELJ(K) * (XDELJ(K) - 1.0) * .25
 1200   CONTINUE
      END IF
C
 2100 CONTINUE
      IF (LIN) THEN
C
C     LINEAR INTERPOLATION
C
      DO 2200 KK = 1,NPTS
        ERAS(KK,2) = (ALOLA(IP1(KK),JY(KK,2))-ALOLA(IV(KK),JY(KK,2)))
     &             * XDELI(KK) + ALOLA(IV(KK),JY(KK,2))
        ERAS(KK,3) = (ALOLA(IP1(KK),JY(KK,3))-ALOLA(IV(KK),JY(KK,3)))
     &             * XDELI(KK) + ALOLA(IV(KK),JY(KK,3))
 2200 CONTINUE
C
      DO 2300 KK = 1,NPTS
        AMERC(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &            * XDELJ(KK)
 2300 CONTINUE
C
      ELSE
C
C     QUADRATIC INTERPOLATION
C
      DO 2400 KK = 1,NPTS
        ERAS(KK,1)=(ALOLA(IP1(KK),JY(KK,1))-ALOLA(IV(KK),JY(KK,1)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,1)) +
     &            ( ALOLA(IM1(KK),JY(KK,1)) - ALOLA(IV(KK),JY(KK,1))
     &            - ALOLA(IP1(KK),JY(KK,1))+ALOLA(IP2(KK),JY(KK,1)))
     &            * XI2TM(KK)
        ERAS(KK,2)=(ALOLA(IP1(KK),JY(KK,2))-ALOLA(IV(KK),JY(KK,2)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,2)) +
     &            ( ALOLA(IM1(KK),JY(KK,2)) - ALOLA(IV(KK),JY(KK,2))
     &            - ALOLA(IP1(KK),JY(KK,2))+ALOLA(IP2(KK),JY(KK,2)))
     &            * XI2TM(KK)
        ERAS(KK,3)=(ALOLA(IP1(KK),JY(KK,3))-ALOLA(IV(KK),JY(KK,3)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,3)) +
     &            ( ALOLA(IM1(KK),JY(KK,3)) - ALOLA(IV(KK),JY(KK,3))
     &            - ALOLA(IP1(KK),JY(KK,3))+ALOLA(IP2(KK),JY(KK,3)))
     &            * XI2TM(KK)
        ERAS(KK,4)=(ALOLA(IP1(KK),JY(KK,4))-ALOLA(IV(KK),JY(KK,4)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,4)) +
     &            ( ALOLA(IM1(KK),JY(KK,4)) - ALOLA(IV(KK),JY(KK,4))
     &            - ALOLA(IP1(KK),JY(KK,4))+ALOLA(IP2(KK),JY(KK,4)))
     &            * XI2TM(KK)
 2400      CONTINUE
C
       DO 2500 KK = 1,NPTS
         AMERC(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &             * XDELJ(KK)  + (ERAS(KK,1) - ERAS(KK,2)
     &             - ERAS(KK,3) + ERAS(KK,4)) * XJ2TM(KK)
 2500  CONTINUE
C
      ENDIF
C
      RETURN
      END