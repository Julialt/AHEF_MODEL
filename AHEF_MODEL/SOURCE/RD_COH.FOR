C=====================================================================
      SUBROUTINE READ_COHORT_RISKS
C=====================================================================
C   Read cohort risk file
c=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      include 'effects.fi'

      real value, lastvalue
      logical eof
      integer yrlp, lastyear, year

      open(iunit, file = cohortname, status = 'OLD', err = 1120)
      write(errfile,*) 'Reading Cohort Risks'

      call skip( iunit, eof )

      lastyear = 0

      do while (.not. eof)
        read( iunit, 100 ) year, value
        if (year .ge. colo_year)
     +    cohort_risk((year-colo_year)/step + 1) = value

        write( errfile, 100 ) year, value
100     format(t5,i4,t15,f5.2)

        if ( (lastyear .ne. 0) .and.
     +       (year .gt. colo_year) .and.
     +       (year .ge. (lastyear + 2 * step)) ) then
          do yrlp = max(colo_year, lastyear + step), year - step, step
            cohort_risk((yrlp-colo_year)/step + 1) =
     +         xinterp(lastvalue, value, lastyear, yrlp, year)
            write(errfile,100) yrlp,
     +             cohort_risk((yrlp-colo_year)/step + 1)
          end do ! yrlp
        endif

        lastyear = year
        lastvalue = value

        call skip( iunit, eof )
      end do

999   close (iunit)
      return

1120  call error(120,*999)
      end

