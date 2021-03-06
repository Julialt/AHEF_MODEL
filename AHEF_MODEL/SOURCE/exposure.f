C=====================================================================
      SUBROUTINE exposure
C=====================================================================
C   This subroutine contains the exposure module.
C   Input:    ozone in dobson(year,month,latitude)
C   Output:   exposure(cohORt,iagey,ilat)
C=====================================================================
      !USE setup

      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'exposure.fi'
      INCLUDE 'setup.h'

!---------------------------------------------------------------
      INTEGER i
      REAL row,col
      CHARACTER*8  indexname
      CHARACTER*12 lookupname, weightname
!      LOGICAL eof, peakflag, first
      LOGICAL peakflag, first

!---------------------------------------------------------------
      WRITE (*,*) 'Running exposure model . . . .'

! INITIALIZE SOME QUANTITIES
      yrlo = 0
      yrhi = 0

      WRITE(*,*)' ozone filename = ',oznname

      OPEN(exprun,file=dir_io//exprunname,status='OLD',err=1050)

C      Defaultfile=
C     +      'C:\Documents and Settings\18959\Desktop\AHEF
C     +\countyAHEF\miniruns\input DATA\',

      WRITE(errfile,*) 'Reading exposure runfile ',exprunname
      WRITE(*,*) 'Reading exposure runfile ',exprunname

      CALL skip(exprun, eof)
        READ (exprun,90) colo_year, cohi_year
90      FORMAT (t35,i4,t43,i4)
      CALL skip(exprun, eof)

      colo = 1
      cohi = (cohi_year - colo_year) / step + 1

      first = .true.

!==========================================================
! loop through all requested  UV indices
!==========================================================
      DO WHILE (.NOT. eof)

! DEBUG !
        IF(EOF) THEN
          WRITE(*,*)"FOUND EOF CONDITION : EXITING READ LOOP!!!"
          EXIT
        ENDIF
! END DEBUG !

        CALL skip(exprun,eof)
        READ(exprun,100,err=1070 )
     +              indexname,lookupname,minmon,maxmon,weightname,drtype
        WRITE(errfile,100) 
     +              indexname,lookupname,minmon,maxmon,weightname,drtype
        CALL skip(exprun,eof)

! 'tn' means "skip to column n"
100     FORMAT(t5,a8,t19,a8,t33,i2,t39,i2,t48,a8,5x,a4)
!100     FORMAT(4x,a8, 6x,a8, 6x,i2, 4x,i2, 7x,a8,5x,a4)

        WRITE(*,*) '==========================================='
        WRITE(*,*) 'indexname   = ', indexname
        WRITE(*,*) '(exp)drtype = ', drtype

! Lookup table: Action Spectrum-weighted Irradiance wrt O3 column (DU) & cosSZA
        lookupname = lookupname(1:len_trim(lookupname)) // '.IRR'

        WRITE(*,*)'lookupname  = ',lookupname

        CALL readlookup(lookupname)
        IF (errflag) GOTO 999

!------------------------------------------------------------------
C Note: The following section reads weighting factors for cohorts    
C       The model is currently configured to weight by age
!------------------------------------------------------------------

!! Lookup table: Age weighting table 
!        weightname = weightname(1:len_trim(weightname)) // '.WGT'
!        WRITE(*,*)'weightname  = ',weightname

!        CALL readweight(weightname)
!        IF (errflag) GOTO 999

C=====================================================================
C Do following section twice: for O3 projection and baseline scenarios
C 1)  Read relevant Ozone distribution
C 2)  Apply Madronich lookup:
C      dobson(yr,mo,lat) -> irr(yr,mo,lat) -> irr(yr,lat)
C 3)  Convert yearly exposure into exposure by cohort and age
C 4)  Write yearly exposure by cohort and age for each scenario
C=====================================================================
        DO i=1,2

          WRITE(*,*) '..........................................'
          SELECT CASE (i)
            CASE (1)
              WRITE(*,*)'Reading and calculating projection exposure'
              expblflag = .false.
              CALL readozone(oznname)

            CASE (2)
              WRITE(*,*)'Reading and calculating baseline exposure'
              expblflag = .true.
              CALL readozone('BASELINE.OZN')

          END SELECT

          IF (errflag) GOTO 999

          CALL calc_irradiance

! SUBROUTINE calc_exposure_by_cohort is in dummy.f
C       CALL calc_exposure_by_cohort

          CALL calc_exposure_by_age

C         CALL writeexpos(indexname,first)
          CALL writexage(indexname,first)

C         IF (?diag?) THEN                 
c           CALL writeirrad                
C         ENDIF

        ENDDO ! i=1,2

        first = .false.

      ENDDO ! WHILE (.NOT. eof)

      WRITE(*,*) '==========================================='

999   CLOSE(exprun)
      RETURN

1050  CALL error(50, *999)
1070  CALL error(70, *999)

      END SUBROUTINE exposure

