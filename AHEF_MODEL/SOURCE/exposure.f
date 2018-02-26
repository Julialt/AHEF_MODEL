C=====================================================================
      SUBROUTINE exposure
C=====================================================================
C   This subroutine contains the exposure module.
C   Input:    ozone in dobson(year,month,latitude)
C   Output:   exposure(cohORt,iagey,ilat)
C=====================================================================

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'exposure.fi'

      REAL row,col
      CHARACTER*8  indexname
      CHARACTER*12 lookupname, weightname
      LOGICAL eof, peakflag, first

      WRITE (*,*) 'Running exposure model . . . .'


      yrlo = 0
      yrhi = 0

c      mrlm
      WRITE(*,*)' ozone filename = ',oznname
c
C     CALL readozone('HISTORIC.OZN')
C     errflag = .false.

      OPEN(exprun, file =exprunname, 
C      Defaultfile=
C     +      'C:\Documents and Settings\18959\Desktop\AHEF
C     +\countyAHEF\miniruns\input DATA\',
     +  status = 'OLD', err = 1050)
      WRITE (errfile,*) 'Reading exposure runfile'
c mrlm for 
cc      WRITE(*,*)'exprunname = ',exprunname
c
      CALL skip(exprun, eof)

      READ (exprun,90) colo_year, cohi_year
90    FORMAT (t35,i4,t43,i4)

      colo = 1
      cohi = (cohi_year - colo_year) / step + 1

      CALL skip(exprun, eof)

      first = .true.
c      mrlm
cc      WRITE(*,*)' ozone filename = ',oznname
c
      DO WHILE (.NOT. eof)
        READ( exprun, 100, err=1070 )
     +                      indexname,lookupname,minmon,maxmon,
     +                      weightname, drtype
        WRITE( errfile, 100 ) indexname,lookupname,minmon,maxmon,
     +                        weightname, drtype
100     FORMAT(t5,a8,t19,a8,t33,i2,t39,i2,t48,a8,5x,a4)

        WRITE(*,'(t1,a12,t14,a4)') '(exp)drtype=', drtype

        lookupname = lookupname(1:len_trim(lookupname)) // '.IRR'
        weightname = weightname(1:len_trim(weightname)) // '.WGT'

c  MRLM - 10/2008 debugging - write out ozone filename
        WRITE(*,*)' reading and calculating scenario case'
cc        WRITE(*,*)'ozone filename is   :',oznname
c
        CALL readozone(oznname)
        IF (errflag) GOTO 999

CMRLM - 10/2008 debug - write out lookupname
         WRITE(*,*)'lookupname is:',lookupname

        CALL readlookup(lookupname)
        IF (errflag) GOTO 999

C Note: The following routine reads weighting factors for cohorts    
C       The model is currently configured to weight by age
C       CALL readweight(weightname)
C       IF (errflag) GOTO 999

C=====================================================================
C  Apply Madronich lookup:
C    dobson(yr,mo,lat) -> irr(yr,mo,lat) -> irr(yr,lat)
C=====================================================================
C MRLM - debug 10/2008, before calc_irr
cc        WRITE(*,*)'before calc irr'
        CALL calc_irradiance
cc        WRITE(*,*)'after calc irr'

C=====================================================================
C  Convert yearly exposure to exposure by cohort and age
C=====================================================================

C       CALL calc_exposure_by_cohort
        CALL calc_exposure_by_age

        expblflag = .false.
C       CALL writeexpos(indexname,first)
        CALL writexage(indexname,first)

C       IF (?diag?) THEN                 
c         CALL writeirrad                
C       ENDIF

C=====================================================================
C  Calculate the baseline exposure (using baseline ozone)
C=====================================================================
      WRITE(*,*)'reading and calculating baseline exposure'

        CALL readozone('BASELINE.OZN')

c
        CALL calc_irradiance

        CALL calc_exposure_by_age

        expblflag = .true.

        CALL writexage(indexname,first)
c
        CALL skip(exprun,eof)
        first = .false.

      ENDDO

999   CLOSE(exprun)
      RETURN

1050  CALL error(50, *999)
1070  CALL error(70, *999)

      END SUBROUTINE exposure

