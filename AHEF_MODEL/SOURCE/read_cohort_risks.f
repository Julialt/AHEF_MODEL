C=====================================================================
      SUBROUTINE read_cohort_risks
C=====================================================================
C   Read cohort risk file
c=====================================================================

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'

      REAL value, lastvalue
      INTEGER yrlp, lastyear, year
!----------------------------------------------------

      OPEN(iunit, file = cohortname, status = 'OLD', err = 1120)
      WRITE(errfile,*) 'Reading Cohort Risks'

      CALL skip( iunit, eof )

      lastyear = 0

      DO WHILE (.not. eof)
        READ( iunit, 100 ) year, value
        IF (year .ge. colo_year)
     +    cohort_risk((year-colo_year)/step + 1) = value

        WRITE( errfile, 100 ) year, value
100     FORMAT(t5,i4,t15,f5.2)

        IF ( (lastyear .ne. 0) .and.
     +       (year .gt. colo_year) .and.
     +       (year .ge. (lastyear + 2 * step)) ) THEN
          DO yrlp = max(colo_year, lastyear + step), year - step, step
            cohort_risk((yrlp-colo_year)/step + 1) =
     +         xinterp(lastvalue, value, lastyear, yrlp, year)
            WRITE(errfile,100) yrlp,
     +             cohort_risk((yrlp-colo_year)/step + 1)
          END DO ! yrlp
        ENDIF

        lastyear = year
        lastvalue = value

        CALL skip( iunit, eof )
      END DO

999   CLOSE (iunit)
      RETURN

1120  CALL error(120,*999)

      END SUBROUTINE read_cohort_risks
