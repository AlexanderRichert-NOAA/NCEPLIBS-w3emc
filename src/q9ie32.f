C> @file
C                .      .    .                                       .
C> SUBPROGRAM:    Q9IE32      CONVERT IBM370 F.P. TO IEEE F.P.
C>   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 90-06-04
C>
C> ABSTRACT: CONVERT IBM370 32 BIT FLOATING POINT NUMBERS TO IEEE
C>   32 BIT TASK 754 FLOATING POINT NUMBERS.
C>
C> PROGRAM HISTORY LOG:
C>   90-06-04  R.E.JONES   CHANGE TO SUN FORTRAN 1.3
C>   90-07-14  R.E.JONES   CHANGE ISHFT TO LSHIFT OR LRSHFT
C>   91-03-09  R.E.JONES   CHANGE TO SiliconGraphics FORTRAN
C>   92-07-20  R.E.JONES   CHANGE TO IBM AIX XL FORTRAN
C>   95-11-15  R.E.JONES   ADD SAVE STATEMENT
C>   98-11-15  gilbert     Specified 4-byte integers for IBM SP
C>
C> USAGE:    CALL Q9IE32(A, B, N, ISTAT)
C>   INPUT ARGUMENT LIST:
C>     A        - REAL*4 ARRAY OF IBM370 32 BIT FLOATING POINT NUMBERS
C>     N        - NUMBER OF POINTS TO CONVERT
C>
C>   OUTPUT ARGUMENT LIST:
C>     B        - REAL*4 ARRAY OF IEEE 32 BIT FLOATING POINT NUMBERS
C>     ISTAT    - NUMBER OF POINT GREATER THAN 10E+38, NUMBERS ARE SET TO
C>                IEEE INFINITY, ONE IS ADDED TO ISTAT. NUMBERS LESS THAN
C>                E-38 ARE SET TO ZERO , ONE IS  NOT ADDED TO ISTAT.
C>
C> REMARKS: SEE IEEE TASK 754 STANDARD FLOATING POINT ARITHMETIC
C>   FOR MORE INFORMATION ABOUT IEEE F.P.
C>
C> ATTRIBUTES:
C>   LANGUAGE: IBM AIX XL FORTRAN Compiler/6000
C>   MACHINE:  IBM RS6000 model 530
C>
      SUBROUTINE Q9IE32(A,B,N,ISTAT)
C
       INTEGER(4)      A(*)
       INTEGER(4)      B(*)
       INTEGER(4)      SIGN
       INTEGER(4) INFIN,MASKFR,MASKSN,MASK21,MASK22,MASK23
       INTEGER(4) ITEMP,ISIGN,IEEEXP,K,LTEMP
C
       SAVE
C
       DATA  INFIN /Z'7F800000'/
       DATA  MASKFR/Z'007FFFFF'/
       DATA  MASKSN/Z'7FFFFFFF'/
       DATA  MASK21/Z'00200000'/
       DATA  MASK22/Z'00400000'/
       DATA  MASK23/Z'00800000'/
       DATA  SIGN  /Z'80000000'/
C
           IF (N.LT.1) THEN
             ISTAT = -1
             RETURN
           ENDIF
C
           ISTAT = 0
C
         DO 40 I = 1,N
           ISIGN = 0
           ITEMP = A(I)
C
C          TEST SIGN BIT
C
           IF (ITEMP.EQ.0) GO TO 30
C
           IF (ITEMP.LT.0) THEN
C
             ISIGN = SIGN
C
C            SET SIGN BIT TO ZERO
C
             ITEMP = IAND(ITEMP,MASKSN)
C
           END IF
C
C
C          CONVERT IBM EXPONENT TO IEEE EXPONENT
C
           IEEEXP = (ISHFT(ITEMP,-24_4) - 64_4) * 4 + 126 
C
           K = 0
C
C          TEST BIT 23, 22, 21
C          ADD UP NUMBER OF ZERO BITS IN FRONT OF IBM370 FRACTION
C
           IF (IAND(ITEMP,MASK23).NE.0) GO TO 10
           K = K + 1
           IF (IAND(ITEMP,MASK22).NE.0) GO TO 10
           K = K + 1
           IF (IAND(ITEMP,MASK21).NE.0) GO TO 10
           K = K + 1
C
 10      CONTINUE
C
C          SUBTRACT ZERO BITS FROM EXPONENT
C
           IEEEXP = IEEEXP - K
C
C          TEST FOR OVERFLOW
C
           IF (IEEEXP.GT.254) GO TO 20
C
C          TEST FOR UNDERFLOW
C
           IF (IEEEXP.LT.1) GO TO 30
C
C          SHIFT IEEE EXPONENT TO BITS 1 TO 8
C
           LTEMP = ISHFT(IEEEXP,23_4)
C
C          SHIFT IBM370 FRACTION LEFT K BIT, AND OUT BITS 0 - 8
C          OR TOGETHER THE EXPONENT AND THE FRACTION
C          OR IN SIGN BIT
C
           B(I)  = IOR(IOR(IAND(ISHFT(ITEMP,K),MASKFR),LTEMP),ISIGN)
C
           GO TO 40
C
 20      CONTINUE
C
C          OVERFLOW , SET TO IEEE INFINITY, ADD 1 TO OVERFLOW COUNTER
C
           ISTAT  = ISTAT + 1
           B(I)   = IOR(INFIN,ISIGN)
           GO TO 40
C
 30      CONTINUE
C
C          UNDERFLOW , SET TO ZERO
C
           B(I)   =  0
C
 40      CONTINUE
C
         RETURN
       END
