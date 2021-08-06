C> @file
C> @brief COMPUTES COEFFICIENTS FOR USE IN W3FA12
C
C> COMPUTES COEFFICIENTS FOR USE IN W3FA12
C> @author SELA,JOE @date 80-10-28
C>
C> SUBROUTINE COMPUTES DOUBLE PRECISION  COEFFICIENTS
C> USED IN GENERATING  LEGENDRE POLYNOMIALS IN SUBR. W3FA12.
C> ON A CRAY DOUBLE PRECISION IS CHANGED TO REAL, DSQRT TO SQRT.
C>
C> PROGRAM HISTORY LOG:
C> -  80-10-28  JOE SELA
C> -  84-06-01  R.E.JONES   CHANGE TO IBM VS FORTRAN
C> -  93-04-12  R.E.JONES   CHANGES FOR CRAY, DOUBLE PRECISION TO REAL
C>
C> @param[out] EPS REAL COEFFICIENTS USED IN
C>                      COMPUTING LEGENDRE POLYNOMIALS.
C>                      DIMENSION OF EPS IS (JCAP+2)*(JCAP+1)
C> @param[in] JCAP ZONAL WAVE NUMBER THIRTY, ETC.
C>
C> SUBPROGRAMS CALLED:
C>     sqrt()
C>
       SUBROUTINE W3FA11 (EPS,JCAP)
C
       REAL  EPS(*)
       REAL  A
C
       SAVE
C
         JCAP1 = JCAP + 1
         JCAP2 = JCAP + 2
C
         DO 100 LL = 1,JCAP1
           L = LL - 1
           JLE = (LL-1) * JCAP2
C
         DO 100 INDE = 2,JCAP2
           N = L + INDE - 1
           A=(N*N-L*L)/(4.0*N*N-1.0)
           EPS(JLE+INDE) = SQRT(A)
 100     CONTINUE
C
         DO 200 LL = 1,JCAP1
           JLE = (LL-1) * JCAP2
           EPS(JLE+1) = 0.0
 200     CONTINUE
C
         RETURN
       END