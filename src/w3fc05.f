C> @file
C> @brief Earth U,V wind components to dir and spd.
C> @author John Stackpole @date 1981-12-30

C> Given the true (Earth oriented) wind components
C> compute the wind direction and speed.
C> Input winds at the pole are assumed to follow the WMO
C> conventions, with the output direction computed in accordance
C> with WMO standards for reporting winds at the pole.
C> (see office note 241 for WMO definition.)
C>
C> @param[in] U REAL Earth-oriented U-component.
C> @param[in] V REAL Earth-oriented V-component.
C> @param[out] DIR REAL Wind direction, degrees. Values will
C> be from 0 to 360 inclusive.
C> @param[out] SPD REAL Wind speed in same units as input.
C>
C> @note If speed is less than 1e-10 then direction will be set to zero.
C>
C> @author John Stackpole @date 1981-12-30
      SUBROUTINE W3FC05(U, V, DIR, SPD)                                                                                                    11700000
C
C     VARIABLES.....
C
      REAL   U, V, DIR, SPD, XSPD
C
C     CONSTANTS.....
C
      DATA  SPDTST/1.0E-10/
      DATA  RTOD  /57.2957795/
      DATA  DCHALF/180.0/
C
      XSPD = SQRT(U * U + V * V)
      IF (XSPD .LT. SPDTST) THEN
         DIR = 0.0
      ELSE
         DIR = ATAN2(U,V) * RTOD + DCHALF + 1.E-3
      ENDIF
      SPD = XSPD
      RETURN
      END
