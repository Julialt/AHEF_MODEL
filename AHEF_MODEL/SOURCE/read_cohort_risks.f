C=====================================================================
      SUBROUTINE read_cohort_risks
C=====================================================================
C   Read cohort risk file
c=====================================================================

      INCLUDE 'files.h'
      INCLUDE 'global.h'
      INCLUDE 'effects.h'

      INTEGER :: yrlp,lastyear,year

      REAL :: rval,lastrval
!----------------------------------------------------

      OPEN(iunit, FILE = cohortname, STATUS = 'OLD', ERR = 1120)
      WRITE(logfile,*) 'Reading Cohort Risks'

      CALL skip( iunit, eof )

      lastyear = 0

      DO WHILE (.NOT. eof)
        READ( iunit, 100 ) year, rval
        IF (year .GE. colo_year)
     +    cohort_risk((year-colo_year)/step + 1) = rval

        WRITE( logfile, 100 ) year, rval
100     FORMAT(t5,i4,t15,f5.2)

        IF ( (lastyear .NE. 0) .AND.
     +       (year .GT. colo_year) .AND.
     +       (year .GE. (lastyear + 2 * step)) ) THEN
          DO yrlp = max(colo_year, lastyear + step), year - step, step
            cohort_risk((yrlp-colo_year)/step + 1) =
     +         xinterp(lastrval, rval, lastyear, yrlp, year)
            WRITE(logfile,100) yrlp,
     +             cohort_risk((yrlp-colo_year)/step + 1)
          END DO ! yrlp
        ENDIF

        lastyear = year
        lastrval = rval

        CALL skip( iunit, eof )
      END DO

999   CLOSE (iunit)
      RETURN

1120  CALL error(120,*999)

      END SUBROUTINE read_cohort_risks
