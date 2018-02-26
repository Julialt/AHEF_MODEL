C=====================================================================
      SUBROUTINE zenith(lat,mon,day,time,zen)
C=====================================================================
C  Subroutine to calculate solar zenith, adapted from Sasha Madronich.
C  Based on equations given by Paltridge and Platt [1976] "Radiative
C    Processes in Meteorology and Climatology", Elsevier, pp. 62,63.
C  Fourier coefficients originally from:
C    Spencer, J.W., 1971, Fourier series representation of the position
C      of the sun, Search, 2:172.
C   NOTE: program is an approximation with no changes from year to year.
C
C  INPUTS:   LAT    - latitude in decimal degrees
C            MON    - Month from 1 to 12
C            DAY    - Day of month
C            TIME   - Decimal military (i.e., 22.75 = 10:45 PM)
C   NOTE:  Time is assumed GMT, and longitude is assumed 0
C
C  OUTPUT:   ZEN    - Zenith angle (0-180)
C=====================================================================

      IMPLICIT REAL*4 (a-h,o-z)

* input data

      REAL long,time
      INTEGER lat,mon,day

* internal data:

      INTEGER imn(12)
      REAL lbut,lzut

      DATA imn/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA  pi / 3.1415926535898 /
c
      dr = pi/180.
      long = 0.0

* convert to radians

      rlt = lat*dr

* compute current (Julian) day of year IJD = 1 to 365

      ijd = 0
      DO 30 i = 1, mon - 1
         ijd = ijd + imn(i)
 30   CONTINUE

c
      ijd = ijd + day

* calculate decimal Julian day from start of year:

      d = FLOAT(ijd-1) + time/24.

* Equation 3.8 for "day-angle"

      tz = 2.*pi*d/365.

* Equation 3.7 For declination in radians

      rdecl = 0.006918 - 0.399912*COS(   tz) + 0.070257*SIN(   tz)
     $                 - 0.006758*COS(2.*tz) + 0.000907*SIN(2.*tz)
     $                 - 0.002697*COS(3.*tz) + 0.001480*SIN(3.*tz)

* Equation 3.11 for Equation of time  in radians

      eqr   = 0.000075 + 0.001868*COS(   tz) - 0.032077*SIN(   tz)
     $                 - 0.014615*COS(2.*tz) - 0.040849*SIN(2.*tz)

* convert equation of time to hours:

      eqh = eqr*24./(2.*pi)

* calculate local hour angle (hours):

      lbut = 12. - eqh - long*24./360

* convert to angle from UT

      lzut = 15.*(time - lbut)
      zpt = lzut*dr

* Equation 2.4 for COSine of zenith angle

      zen = SIN(rlt)*SIN(rdecl) + COS(rlt)*COS(rdecl)*COS(zpt)

      RETURN

      END SUBROUTINE zenith
