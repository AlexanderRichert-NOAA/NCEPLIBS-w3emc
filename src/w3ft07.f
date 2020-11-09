C> @file
C                .      .    .                                       .
C> SUBPROGRAM:  W3FT07        TRANSFORM GRIDPOINT FLD BY INTERPOLATION
C>   PRGMMR: LIN              ORG: NMC412      DATE:93-03-24
C>
C> ABSTRACT: TRANSFORMS DATA CONTAINED IN A GIVEN GRID ARRAY
C>   BY TRANSLATION, ROTATION ABOUT A COMMON POINT AND DILATATION
C>   IN ORDER TO CREATE A NEW GRID ARRAY ACCORDING TO SPECS.
C>
C> PROGRAM HISTORY LOG:
C>   74-09-01  ORIGINAL AUTHOR(S): J. MCDONELL, J.HOWCROFT
C>   84-06-27  R.E.JONES   CHANGE TO IBM VS FORTRAN
C>   89-01-24  R.E.JONES   CHANGE TO MICROSOFT FORTRAN 4.10
C>   89-03-31  R.E.JONES   CHANGE TO VAX-11 FORTRAN
C>   93-03-16  D. SHIMOMURA -- RENAMED FROM W3FT00() TO W3FT07()
C>                IN ORDER TO MAKE MINOR MODS WHILE DOING F77.
C>                CHANGES TO CALL SEQUENCE; CHANGES TO VRBL NAMES;
C>                ADDED COMMENTS.
C>
C>                   ...  1    2  3   4      5      6      7      8
C> USAGE:    CALL W3FT07(FLDA,IA,JA,AIPOLE,AJPOLE,BIPOLE,BJPOLE,DSCALE,
C>                       ANGLE,LINEAR,LDEFQQ,DEFALT,FLDB,IB,JB)
C>                        9      10     11     12    13  14 15
C>   INPUT ARGUMENT LIST:
C>     FLDA(IA,JA)   - REAL*4 ORIGINAL SOURCE GRID-POINT DATA FIELD
C>     AIPOLE,AJPOLE - REAL*4 COMMON POINT I- AND J-COORDINATES OF THE
C>                     ORIGINAL FIELD, ASSUMING A RIGHT-HAND CARTESIAN
C>                     COORDINATE SYSTEM. THE POINT NEED NOT BE INSIDE
C>                     THE BOUNDS OF EITHER GRID
C>                     AND CAN HAVE FRACTIONAL VALUES.
C>                     COMMON POINT ABOUT WHICH TO ROTATE THE GRIDPOINTS
C>     BIPOLE,BJPOLE - REAL*4 COMMON POINT I- AND J-COORDINATES FOR
C>                     TRANSFORMED DESTINATION GRID
C>     DSCALE - REAL*4 SCALE-CHANGE (DILATION) EXPRESSED AS
C>                     A RATIO OF THE TRANSFORMED FIELD TO THE ORIGINAL
C>                     FIELD
C>                     DSCALE = GRDLENKM(DESTINATION) / GRDLENKM(SOURCE)
C>
C>     ANGLE  - REAL*4 DEGREE MEASURE OF THE ANGLE REQUIRED TO
C>                     ROTATE THE J-ROW OF THE ORIGINAL GRID INTO
C>                     COINCIDENCE WITH THE NEW GRID. (+ COUNTER-
C>                     CLOCKWISE, - CLOCKWISE)
C>                     ANGLE = VERTLONW(SOURCE) - VERTLONW(DESTINATION)
C>
C>     LINEAR - LOGICAL*4 INTERPOLATION-METHOD SELECTION SWITCH:
C>                      .TRUE.   BI-LINEAR INTERPOLATION
C>                      .FALSE.  BI-QUADRATIC INTERPOLATION
C>
C>     LDEFQQ - LOGICAL*4 DEFAULT-VALUE SWITCH:
C>                      IF .TRUE. THEN
C>                        USE DEFAULT-VALUE FOR DESTINATION POINT
C>                        OUT-OF-BOUNDS OF GIVEN GRID;
C>                      ELSE
C>                        EXTRAPOLATE COARSELY FROM NEARBY BNDRY POINT
C>
C>     DEFALT - REAL*4  THE DEFAULT-VALUE TO USE IF LDEFQQ = .TRUE.
C>
C>   OUTPUT ARGUMENT LIST:
C>     FLDB(IB,JB) - REAL*4 RESULTING TRANSFORMED DESTINATION FIELD
C>
C>
C> REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C>     IN GENERAL 'FLDA' AND 'FLDB' CANNOT BE EQUIVALENCED
C>     ALTHOUGH THERE ARE SITUATIONS IN WHICH IT WOULD BE SAFE TO DO
C>     SO. CARE SHOULD BE TAKEN THAT ALL OF THE NEW GRID POINTS LIE
C>     WITHIN THE ORIGINAL GRID, NO ERROR CHECKS ARE MADE.
C>
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN 77
C>   MACHINE:  CRAY Y-MP8/864
C>
      SUBROUTINE W3FT07(FLDA,IA,JA,AIPOLE,AJPOLE,BIPOLE,BJPOLE,
     A                   DSCALE,ANGLE,LINEAR,LDEFQQ,DEFALT,FLDB,IB,JB)
C
      REAL      FLDA(IA,JA)
      REAL      AIPOLE,AJPOLE
      REAL      BIPOLE,BJPOLE
      REAL      DSCALE
      REAL      ANGLE
      REAL      DEFALT
      REAL      FLDB(IB,JB)
      REAL      ERAS(4)
      REAL      TINY
C
      LOGICAL   LINEAR
      LOGICAL   LDEFQQ
C
      SAVE
C
      DATA  TINY  / 0.001 /
C
C     ... WHERE TINY IS IN UNITS OF 1.0 = 1 GRID INTERVAL
C
C     . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .
C
      THETA = ANGLE * (3.14159/180.)
      SINT  = SIN (THETA)
      COST  = COS (THETA)
C
C     ... WE WILL SCAN ALONG THE J-ROW OF THE DESTINATION GRID ...
      DO 288 JN = 1,JB
        BRELJ  = FLOAT(JN) - BJPOLE
C
        DO 277 IN = 1,IB
          BRELI = FLOAT(IN) - BIPOLE
          STI = AIPOLE + DSCALE*(BRELI*COST - BRELJ*SINT)
          STJ = AJPOLE + DSCALE*(BRELI*SINT + BRELJ*COST)
          IM = STI
          JM = STJ
C
C         ... THE PT(STI,STJ) IS THE LOCATION OF THE FLDB(IN,JN)
C         ... IN FLDA,S COORDINATE SYSTEM
C         ... IS THIS POINT LOCATED OUTSIDE FLDA?
C         ...           ON THE BOUNDARY LINE OF FLDA?
C         ...           ON THE FIRST INTERIOR GRIDPOINT OF FLDA?
C         ...           GOOD INSIDER, AT LEAST 2 INTERIOR GRIDS INSIDE?
          IOFF = 0
          JOFF = 0
          KQUAD = 0
C
          IF (IM .LT. 1) THEN
C           ... LOCATED OUTSIDE OF FLDA, OFF LEFT SIDE ...
            II = 1
            IOFF = 1
          ELSE IF (IM .EQ. 1) THEN
C           ... LOCATED ON BOUNDARY OF FLDA, ON LEFT EDGE ...
            KQUAD = 5
          ELSE
C           ...( IM .GT. 1) ... LOCATED TO RIGHT OF LEFT-EDGE ...
            IF ((IA-IM) .LT. 1) THEN
C             ... LOCATED OUTSIDE OF OR EXACTLY ON RIGHT EDGE OF FLDA ..
              II = IA
              IOFF = 1
            ELSE IF ((IA-IM) .EQ. 1) THEN
C             ... LOCATED ON FIRST INTERIOR PT WITHIN RIGHT EDGE OF FLDA
              KQUAD = 5
            ELSE
C             ... (IA-IM) IS .GT. 1) ...GOOD INTERIOR, AT LEAST 2 INSIDE
            ENDIF
          ENDIF
C
C         . . . . . . . . . . . . . . .
C
          IF (JM .LT. 1) THEN
C           ... LOCATED OUTSIDE OF FLDA, OFF BOTTOM ...
            JJ = 1
            JOFF = 1
          ELSE IF (JM .EQ. 1) THEN
C           ... LOCATED ON BOUNDARY OF FLDA, ON BOTTOM EDGE ...
            KQUAD = 5
          ELSE
C           ...( JM .GT. 1) ... LOCATED ABOVE BOTTOM EDGE ...
            IF ((JA-JM) .LT. 1) THEN
C             ... LOCATED OUTSIDE OF OR EXACTLY ON TOP EDGE OF FLDA ..
              JJ = JA
              JOFF = 1
            ELSE IF ((JA-JM) .EQ. 1) THEN
C             ... LOCATED ON FIRST INTERIOR PT WITHIN TOP EDGE OF FLDA
              KQUAD = 5
            ELSE
C             ... ((JA-JM) .GT. 1) ...GOOD INTERIOR, AT LEAST 2 INSIDE
            ENDIF
          ENDIF
C
          IF ((IOFF + JOFF) .EQ. 0) THEN
            GO TO 244
          ELSE IF ((IOFF + JOFF) .EQ. 2) THEN
            GO TO 233
          ENDIF
C
          IF (IOFF .EQ. 1) THEN
            JJ = STJ
          ENDIF
          IF (JOFF .EQ. 1) THEN
            II = STI
          ENDIF
  233     CONTINUE
          IF (LDEFQQ) THEN
            FLDB(IN,JN) = DEFALT
          ELSE
            FLDB(IN,JN) = FLDA(II,JJ)
          ENDIF
          GO TO 277
C
C         . . . . . . . . . . . . .
C
  244     CONTINUE
          I = STI
          J = STJ
          XDELI = STI - FLOAT(I)
          XDELJ = STJ - FLOAT(J)
C
          IF ((ABS(XDELI) .LT. TINY) .AND. (ABS(XDELJ) .LT. TINY)) THEN
C           ... THIS POINT IS RIGHT AT A GRIDPOINT. NO INTERP NECESSARY
            FLDB(IN,JN) = FLDA(I,J)
            GO TO 277
          ENDIF
C
          IF ((KQUAD .EQ. 5) .OR. (LINEAR)) THEN
C           ... PERFORM BI-LINEAR INTERP ...
            ERAS(1) = FLDA(I,J)
            ERAS(4) = FLDA(I,J+1)
            ERAS(2) = ERAS(1) + XDELI*(FLDA(I+1,J) - ERAS(1))
            ERAS(3) = ERAS(4) + XDELI*(FLDA(I+1,J+1) - ERAS(4))
            DI = ERAS(2) + XDELJ*(ERAS(3) - ERAS(2))
            GO TO 266
C
          ELSE
C           ... PERFORM BI-QUADRATIC INTERP ...
            XI2TM = XDELI * (XDELI-1.) * 0.25
            XJ2TM = XDELJ * (XDELJ-1.) * 0.25
            J1 = J - 1
            DO 255 K=1,4
              ERAS(K)=(FLDA(I+1,J1)-FLDA(I,J1))*XDELI+FLDA(I,J1)+
     A        (FLDA(I-1,J1)-FLDA(I,J1)-FLDA(I+1,J1)+FLDA(I+2,J1))*XI2TM
              J1 = J1 + 1
  255       CONTINUE
C
            DI = ERAS(2) +  XDELJ*(ERAS(3)-ERAS(2)) +
     A                      XJ2TM*(ERAS(4)-ERAS(3)-ERAS(2)+ERAS(1))
            GO TO 266
          ENDIF
C
  266     CONTINUE
          FLDB(IN,JN) = DI
  277   CONTINUE
  288 CONTINUE
C
      RETURN
      END
