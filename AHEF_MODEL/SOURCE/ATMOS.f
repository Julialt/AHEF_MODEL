C=====================================================================
      PROGRAM ATMOS
C=====================================================================
C  Atmospheric and Health Effects Framework Model  
C  A tool for estimating the impact of emissions of ozone-depleting
C  chemicals on human health and the environment.
C
C  This framework has three major components:
C     1. Atmospheric model:  ODS emis -> Ozone depletion    : ATMOS.f
C                    calls:  calc_EESC.f
C                            calc_ozone.f
C     2. Exposure model   :  Ozone depletion -> UV exposure : exposure.f
C                    calls:  read_lookup.f (action spectrum-wt irradiance)
C                            read_ozone.f
C                            calc_irradiance.f
C                            calc_exposure_by_age.f
C     3. Effects model    :  UV Exposure -> Health effects  : effects.f
C                    calls:  read_population.f
C                            read_exposure_age.f
C                            write_(various).f
C
C=====================================================================
C PURPOSE OF THIS PROGRAM UNIT: File: ATMOS.f
C     - read run file ATMOS.RUN
C     - extract names of input files
C     - extract desired latitude range for calculation
C     - pass file names to subroutines
C
C=====================================================================
C NAMES USED IN THIS PROGRAM UNIT:
C
C=====================================================================

      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'setup.h'

      INTEGER i,j

      CHARACTER(len=50) :: filename,emisfile,odsfile,pertfile,line
      CHARACTER(len=20) :: scenario_a,scenario_b,scenario_d
      CHARACTER(len=4) :: key
      CHARACTER(len=3) :: atmrun_ext

C=====================================================================
C  Initialize 
C=====================================================================
      eof = .false.
      scenario_b = ""
      scenario_d = ""

C=====================================================================
C  Open global runfile and global default names and extensions
C=====================================================================

! hardwire here: later will pass in from AHEF.f
      atmrun_ext = "INP"

      filename = 'ATM_RUN.'//atmrun_ext

      OPEN(atmrun,FILE=dir_in//filename,status='OLD',ERR=999)
      WRITE(*,*)"OPENING runfile", dir_in//filename

      CALL skip(atmrun,eof)

! read ODS species data filename from ATMOS.RUN

      READ(atmrun,'(a)',err=998) line
      CALL skip(atmrun,eof)

      odsfile = ADJUSTL(line)

      WRITE(logfile,*)  "ODS data file = ",odsfile
      !PRINT*,           "ODS data file = ",odsfile

!------------------------------------------------------------
! read global baseline emissions data filename from ATMOS.RUN
! set string "scenario_b" based on global emissions filename
! calculate EESC for scenario_b

      READ(atmrun,'(a50)',err=998) line
      CALL skip(atmrun,eof)
      !PRINT*,line

      emisfile = ADJUSTL(line)

      WRITE(logfile,*)  "emissions file = ",emisfile
      !PRINT*,           "emissions file = ",emisfile

      i=INDEX(emisfile,".")-1
      scenario_b(1:i)=emisfile(1:i)

      CALL calc_EESC(odsfile,emisfile,scenario_b)

!      PRINT*,"returning from calc_EESC: baseline scenario = "
!     &                                , scenario_b

!------------------------------------------------------------
! read ATMOS.RUN for keyword 
!      if keyword = "DELT" then
! read emissions perturbation (difference) data filename from ATMOS.RUN
! read perturbation data file and baseline data file and add them
! to give global alternative (projection) emissions file "scenario_a"
!      if keyword = "PROJ" then
! read global alternative (projection) emissions filename from ATMOS.RUN
!------------------------------------------------------------

      READ(atmrun,'(a50)',err=998) line
      CALL skip(atmrun,eof)
      !PRINT*,line

      CLOSE(atmrun)

      line = ADJUSTL(line)
      j=INDEX(line," ")-1
      key = line(1:j)

      line = ADJUSTL(line(j+1:))
      j=INDEX(line," ")-1


      IF(key.EQ."PROJ")THEN
! set string "scenario_a" based on global emissions filename

        emisfile = line(1:j)
        !PRINT*,"emisfile = ",emisfile

        j=INDEX(emisfile,".")-1
        scenario_a(1:j)=emisfile(1:j)

      ELSEIF(key.EQ."DELT")THEN
! set string "scenario_d" based on perturbation emissions filename
! and set string "scenario_a" = "scenario_b"+"scenario_d"
! and combine (add) baseline and perturbation files

        pertfile = line(1:j)
        !PRINT*,"pertfile = ",pertfile

        i=INDEX(scenario_b," ")-1
        j=INDEX(pertfile,".")-1
        scenario_d(1:j)=pertfile(1:j)
        scenario_a=scenario_b(1:i)//"+"//scenario_d(1:j)

        CALL combine_emissions(emisfile,pertfile,scenario_a)

! reassign name "emisfile" to refer to new combined emissions file

        i=INDEX(scenario_a," ")-1
        emisfile = scenario_a(1:i)//".EMI"
        !PRINT*,"scenario_a file = ",emisfile

      ELSE
! if bad keyword in instructions file
        WRITE(logfile,*)"unknown keyword in ATM_RUN file: ",key
        STOP
      ENDIF ! (key.EQ."DELT")

! calculate EESC for scenario_a

      CALL calc_EESC(odsfile,emisfile,scenario_a)

!      PRINT*,"returning from calc_EESC: alternate scenario = "
!     &                                , scenario_a

!------------------------------------------------------------
! calculate ozone for baseline scenario

      CALL calc_ozone(scenario_b)

!      PRINT*,"returning from calc_ozone: ",scenario_b

!------------------------------------------------------------
! calculate ozone for alternate / augmented scenario

      CALL calc_ozone(scenario_a)

!      PRINT*,"returning from calc_ozone: ",scenario_a

!------------------------------------------------------------
! END OF PROGRAM
      RETURN

! error conditions
998   PRINT*,"problem reading filename :",line
      STOP
999   PRINT*,"no such file : ",filename
      STOP

      END PROGRAM ATMOS

