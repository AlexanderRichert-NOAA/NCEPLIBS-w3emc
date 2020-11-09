C> @file
C
C> FUNCTION: IW3PDS           TEST FOR MATCH OF TWO PDS
C>   AUTHOR: JONES, R.E.      ORG: W342       DATE: 88-02-22
C>
C> ABSTACT: TEST TWO PDS (GRIB PRODUCT DEFINITION SECTION) TO SEE
C>     IF ALL EQUAL; OTHERWISE .FALSE. IF KEY = 1, ALL 24 CHARACTERS
C>     ARE TESTED, IF KEY = 0 , THE DATE (CHARACTERS 13-17) ARE NOT
C>     TESTED. IF KEY = 2, 11 OF 1ST 12 BYTES ARE TESTED. BYTE 4 IS
C>     IS NOT TESTED, SO TABLE VERSION NUMBER CAN CHANGE AND YOUR
C>     PROGRAM WILL STILL WORK. IF KEY=3, TEST BYTES 1-3, 7-12.
C>
C> PROGRAM HISTORY LOG:
C>   88-02-22  R.E.JONES
C>   89-08-29  R.E.JONES  ADD ENTRY IW3PDS, AN ALIAS NAME.
C>   89-08-29  R.E.JONES  CHANGE TO CRAY CFT77 FORTRAN, MAKE IW3PDS
C>                        THE FUNCTION NAME, IW3PDB THE ALIAS.
C>   94-02-10  R.E.JONES  ADD KEY=2, TEST ONLY 11 OF 1ST 12 BYTES.
C>                        BYTE 4 (TABLE VERSION NO.) IS NOT TESTED
C>   94-07-07  R.E.JONES  ADD KEY=3, TEST BYTES 1-3, 7-12.
C>
C> USAGE:  II = IW3PDS(L1,L2,KEY)
C>         II = IW3PDB(L1,L2,KEY)    ALIAS
C>
C>   INPUT VARIABLES:
C>     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C>     ------ --------- -----------------------------------------------
C>     L1     ARG LIST  CHARACTER ARRAY TO MATCH WITH L2,
C>                      L1 CAN ALSO BE A 3 WORD INTEGER ARRAY
C>     L2     ARG LIST  CHARACTER ARRAY TO MATCH WITH L1,
C>                      L2 CAN ALSO BE A 3 WORD INTEGER ARRAY
C>     KEY    ARG LIST  0, DO NOT INCLUDE THE DATE (BYTES 13-17) IN
C>                         MATCH.
C>                      1, MATCH 24 BYTES OF PDS
C>                      2, MATCH BYTES 1-3, 5-12 OF PDS
C>                      3, MATCH BYTES 1-3, 7-12 OF PDS
C>
C>   OUTPUT VARIABLES:
C>     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C>     ------ --------- -----------------------------------------------
C>     IW3PDB FUNCTION  LOGICAL .TRUE. IF L1 AND L2 MATCH ON ALL CHAR.,
C>                      LOGICAL .FALSE. IF NOT MATCH ON ANY CHAR.
C>
C> EXAMPLE:  SEARCH IDTBL FOR MATCH WITH GIVEN (PDS), USE RBA IN 7TH
C>           ID WORD TO READ RECORD BY RBA.
C>
C>           INTEGER   IDTBL(1794), IPDS(6), RBA
C>           LOGICAL   IW3PDS
C>
C>           KEY = 0
C>           DO 400 I = 9,1793,7
C>             IF (IDTBL(I).EQ.0) GO TO 500
C>               IF (IW3PDS(IPDS,IDTBL(I),KEY)) THEN
C>                  RBA = IDTBL(I+6)
C>                  GO TO 600
C>               END IF
C>   400     CONTINUE
C>
C>   500     CONTINUE
C>           GO TO XXXX ... ERROR EXIT , CAN NOT FIND RECORD
C>
C>   600     ..  READ RECORD WITH RBA
C>
C> REMARK:  ALIAS ADDED BECAUSE OF NAME CHANGE IN GRIB WRITE UP.
C>   NAME OF PDB (PRODUCT DEFINITION BLOCK) WAS CHANGD TO PDS
C>   (PRODUCT DEFINITION SECTION).
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN
C>   MACHINE:  CRAY C916-128, CRAY Y-MP8/864, CRAY Y-MP EL2/256
C>
      LOGICAL FUNCTION IW3PDS(L1, L2, KEY)
C
       CHARACTER*1 L1(24)
       CHARACTER*1 L2(24)
C
       LOGICAL     IW3PDB
C
       SAVE
C
        IW3PDS = .TRUE.
C
        IF (KEY.EQ.1) THEN
          DO 10 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 70
   10     CONTINUE
C
          DO 20 I = 5,24
            IF (L1(I).NE.L2(I))  GO TO 70
   20     CONTINUE
C
        ELSE
C
          DO 30 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 70
   30     CONTINUE
C
C         DO NOT TEST BYTE 4, 5, 6 PDS VER. NO., COUNTRY
C         MODEL NUMBER. U.S., U.K., FNOC WAFS DATA WILL
C         WORK.
C
          IF (KEY.EQ.3) THEN
            DO I = 7,12
              IF (L1(I).NE.L2(I)) GO TO 70
            END DO
            GO TO 60
          END IF
C
C         DO NOT TEST PDS VERSION NUMBER, IT MAY BE 1 O 2
C
          DO 40 I = 5,12
            IF (L1(I).NE.L2(I))  GO TO 70
   40     CONTINUE
          IF (KEY.EQ.2) GO TO 60
C
          DO 50 I = 18,24
            IF (L1(I).NE.L2(I))  GO TO 70
   50     CONTINUE
        ENDIF
C
   60     CONTINUE
          RETURN
C
   70     CONTINUE
            IW3PDS = .FALSE.
            RETURN
C
      ENTRY IW3PDB (L1, L2, KEY)
C
          IW3PDB = .TRUE.
C
        IF (KEY.EQ.1) THEN
          DO 80 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 140
   80     CONTINUE
C
          DO 90 I = 5,24
            IF (L1(I).NE.L2(I))  GO TO 140
   90     CONTINUE
C
        ELSE
C
          DO 100 I = 1,3
            IF (L1(I).NE.L2(I))  GO TO 140
  100     CONTINUE
C
C         DO NOT TEST BYTE 4, 5, 6 PDS VER. NO., COUNTRY
C         MODEL NUMBER. U.S., U.K., FNOC WAFS DATA WILL
C         WORK.
C
          IF (KEY.EQ.3) THEN
            DO I = 7,12
              IF (L1(I).NE.L2(I)) GO TO 140
            END DO
            GO TO 130
          END IF
C
C         DO NOT TEST PDS VERSION NUMBER, IT MAY BE 1 O 2
C
          DO 110 I = 5,12
            IF (L1(I).NE.L2(I))  GO TO 140
  110     CONTINUE
          IF (KEY.EQ.2) GO TO 130
C
          DO 120 I = 18,24
            IF (L1(I).NE.L2(I))  GO TO 140
  120     CONTINUE
        ENDIF
C
  130     CONTINUE
          RETURN
C
  140     CONTINUE
            IW3PDB = .FALSE.
            RETURN
          END
