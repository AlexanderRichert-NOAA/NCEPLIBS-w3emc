C> @file
C                .      .    .                                       .
C> SUBPROGRAM:    XSTORE      STORES A CONSTANT VALUE INTO AN ARRAY
C>   PRGMMR: KEYSER           ORG: W/NMC22    DATE: 07-02-92
C>
C> ABSTRACT: STORES AN 8-BYTE (FULLWORD) VALUE THROUGH CONSECUTIVE
C>   STORAGE LOCATIONS.  (MOVING IS ACCOMPLISHED WITH A DO LOOP.)
C>
C> PROGRAM HISTORY LOG:
C>   92-07-02  D. A. KEYSER (W/NMC22)
C>   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C>
C> USAGE:    CALL XSTORE(COUT,CON,MWORDS)
C>   INPUT ARGUMENT LIST:
C>     CON      - CONSTANT TO BE STORED INTO "MWORDS" CONSECUTIVE
C>                FULLWORDS BEGINNING WITH "COUT" ARRAY
C>     MWORDS   - NUMBER OF FULLWORDS IN "COUT" ARRAY TO STORE "CON";
C>                MUST BE .GT. ZERO (NOT CHECKED FOR THIS)
C>
C>   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C>     COUT     - STARTING ADDRESS FOR ARRAY OF "MWORDS" FULLWORDS
C>                SET TO THE CONTENTS OF THE VALUE "CON"
C>
C> REMARKS: THE VERSION OF THIS SUBROUTINE ON THE HDS COMMON LIBRARY
C>   IS NAS-SPECIFIC SUBR. WRITTEN IN ASSEMBLY LANG. TO ALLOW FAST
C>   COMPUTATION TIME.  SUBR. PLACED IN CRAY W3LIB TO ALLOW CODES TO
C>   COMPILE ON BOTH THE HDS AND CRAY MACHINES.
C>   SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN
C>   MACHINE:  CRAY Y-MP8/864
C>
      SUBROUTINE XSTORE(COUT,CON,MWORDS)
C
      DIMENSION  COUT(*)
C
      DO 1000  I = 1,MWORDS
         COUT(I) = CON
1000  CONTINUE
C
      RETURN
      END
