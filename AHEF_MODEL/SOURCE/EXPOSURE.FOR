C=====================================================================
      SUBROUTINE EXPOSURE
C=====================================================================
C   This subroutine contains the exposure model.
C   Input:    ozone in dobson(year,month,latitude)
C   Output:   exposure(cohort,iagey,ilat)
C=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      include 'exposure.fi'

      real row,col
      character*8  indexname
      character*12 lookupname, weightname
      logical eof, peakflag, first

      write (*,*) 'Running exposure model . . . .'


      yrlo = 0
      yrhi = 0

c	mrlm
	write(*,*)' ozone filename = ',oznname
c
C     call readozone('HISTORIC.OZN')
C     errflag = .false.

      open(exprun, file =exprunname, 
C	Defaultfile=
C     +	'C:\Documents and Settings\18959\Desktop\AHEF
C     +\countyAHEF\miniruns\input data\',
     +  status = 'OLD', err = 1050)
      write (errfile,*) 'Reading exposure runfile'
c mrlm for 
cc	write(*,*)'exprunname = ',exprunname
c
      call skip(exprun, eof)

      read (exprun,90) colo_year, cohi_year
90    format (t35,i4,t43,i4)

      colo = 1
      cohi = (cohi_year - colo_year) / step + 1

      call skip(exprun, eof)

      first = .true.
c	mrlm
cc	write(*,*)' ozone filename = ',oznname
c
      do while (.not. eof)
        read( exprun, 100, err=1070 )
     +                      indexname,lookupname,minmon,maxmon,
     +                      weightname, drtype
        write( errfile, 100 ) indexname,lookupname,minmon,maxmon,
     +                        weightname, drtype
100     format(t5,a8,t19,a8,t33,i2,t39,i2,t48,a8,5x,a4)

        write(*,'(t1,a12,t14,a4)') '(exp)drtype=', drtype

        lookupname = lookupname(1:len_trim(lookupname)) // '.IRR'
        weightname = weightname(1:len_trim(weightname)) // '.WGT'

c  MRLM - 10/2008 debugging - write out ozone fil ename
	  write(*,*)' reading and calculating scenario case'
cc	  write(*,*)'ozone filename is   :',oznname
c
        call readozone(oznname)
        if (errflag) goto 999

CMRLM - 10/2008 debug - write out lookupname
 	  write(*,*)'lookupname is:',lookupname

        call readlookup(lookupname)
        if (errflag) goto 999

C Note: The following routine reads weighting factors for cohorts    
C       The model is currently configured to weight by age
C       call readweight(weightname)
C       if (errflag) goto 999

C=====================================================================
C  Apply Madronich lookup:
C    dobson(yr,mo,lat) -> irr(yr,mo,lat) -> irr(yr,lat)
C=====================================================================
C MRLM - debug 10/2008, before calc_irr
cc	  Write(*,*)'before calc irr'
        call calc_irradiance
cc	  Write(*,*)'after calc irr'

C=====================================================================
C  Convert yearly exposure to exposure by cohort and age
C=====================================================================

C       call calc_exposure_by_cohort
        call calc_exposure_by_age

        expblflag = .false.
C       call writeexpos(indexname,first)
        call writexage(indexname,first)

C       if (?diag?) then                 
c         call writeirrad                
C       endif

C=====================================================================
C  Calculate the baseline exposure (using baseline ozone)
C=====================================================================
	write(*,*)'reading and calculating baseline exposure'

        call readozone('BASELINE.OZN')

c
        call calc_irradiance

        call calc_exposure_by_age

        expblflag = .true.

        call writexage(indexname,first)
c
        call skip(exprun,eof)
        first = .false.

      end do

999   close(exprun)
      return

1050  call error(50, *999)
1070  call error(70, *999)
      end

