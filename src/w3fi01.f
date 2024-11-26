C> @file
C> @brief Determines machine word length in bytes.
C> @author R. Kistler @date 1992-01-10

C> Determines the number of bytes in a full word for the
C> particular machine (IBM or cray).
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author R. Kistler @date 1992-01-10
      SUBROUTINE W3FI01(LW)
C
      INTEGER      LW
      LW=BIT_SIZE(LW)
      LW=LW/8
      RETURN
      END
