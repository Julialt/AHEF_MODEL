      SUBROUTINE combine_emissions(emisfile,pertfile,scenario_a)
!=====================================================================!
! PURPOSE: read ODS baseline and perturbation emissions files,
!          and add them to produce a new file
! AUTHOR:  Julia Lee-Taylor, NCAR
! UPDATE: April 17, 2018
! NOTES: 
!=====================================================================!
! LIST OF SOME VARIABLES:
!
!--------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'global.h'
      INCLUDE 'files.h'
      INCLUDE 'setup.h'
  
!-----------------------
! input variables
      CHARACTER(len=50) :: pertfile,emisfile
      CHARACTER(len=20) :: scenario_a

!--------------------
! internal variables
      INTEGER,PARAMETER :: nods=20 ! # of O3-Depleting Substances
      CHARACTER(len=2) :: cnods="20"
      INTEGER,PARAMETER :: myr=300 ! max # of years considered

      REAL,DIMENSION(nods,myr) :: emis,pert
      CHARACTER(len=10) :: units

      INTEGER :: i,j,isp,iyr,tyr,i1,i2
      INTEGER :: yfirst,ylast,nyr,pyfirst,pylast,npyr
      INTEGER,DIMENSION(myr) :: year,pyr
      INTEGER,DIMENSION(nods) :: invals1,invals2
      CHARACTER(len=50) :: filename
      CHARACTER(len=10),DIMENSION(nods) :: nam1,nam2

!----------------------------------------------------------------------!
! initialise
      
!----------------------------------------------------------------------!
! open and read baseline emissions file

      filename = dir_ems//emisfile
      OPEN(UNIT=iunit,FILE=filename,STATUS="OLD",ERR=999)
      WRITE(logfile,*)'Reading EMISSION data file ',filename
      CALL skip(iunit,eof)

! read the header... units string (expecting "T.yr" or "kT/yr"
      READ(iunit,*) units
      CALL skip(iunit,eof)

! read the header... column numbers
      READ(iunit,*) invals1
      CALL skip(iunit,eof)

! read the header... species names
      READ(iunit,*)nam1
      CALL skip(iunit,eof)

! read the file... year, emissions by species
      iyr = 0
      DO WHILE (.NOT. eof)
        iyr = iyr+1
        READ(iunit,*) year(iyr),emis(1:nods,iyr)

! convert emissions to kT if necessary (based on "units" string)
        IF(units(1:1).EQ."k".OR.units(1:1).EQ."K")THEN
          ! no conversion needed
        ELSE
          emis(1:nods,iyr) = emis(1:nods,iyr) / 1e3
        ENDIF !(units(1:1).EQ."k".OR.units(1:1).EQ."K")THEN

! position file at next data line (ie not beginning with '*')
        CALL skip (iunit,eof)

      END DO !WHILE (.NOT. eof)

      CLOSE(iunit) 

      nyr = iyr
      yfirst = year(1)
      ylast = year(iyr)

!----------------------------------------------------------------------!
! open and write combined emissions file
      i=INDEX(scenario_a," ")-1
      filename = dir_ems//scenario_a(1:i)//".EMI"
      OPEN(UNIT=ounit,FILE=filename,STATUS="UNKNOWN",ERR=999)

! write first lines of header
      WRITE(ounit,'(a)')"*Combined emissions file. Sources:"
      WRITE(ounit,'(a1,a20)')"*",emisfile
      WRITE(ounit,'(a1,a20)')"*",pertfile
      WRITE(ounit,'(a)')"*---------------------------------"
      WRITE(ounit,'(a)')"*-UNITS: use T/yr or kT/yr--------"
      WRITE(ounit,'(a)')"   kT/yr "

!----------------------------------------------------------------------!
! open and read perturbation emissions file
      filename = dir_ems//pertfile
      OPEN(UNIT=iunit,FILE=filename,STATUS="OLD",ERR=999)
      WRITE(logfile,*)'Reading PERTURBATION data file ',filename
      CALL skip(iunit,eof)

! read the header... units string (expecting "T/yr" or "kT/yr")
      READ(iunit,*) units
      CALL skip(iunit,eof)

! read/write the header... column numbers
      READ(iunit,*) invals2
      WRITE(ounit,'(a)')"*-species numbers-----------------"
      WRITE(ounit,'('//cnods//'(i4,7x))') invals2
      CALL skip(iunit,eof)

! read/write the header... species names
      READ(iunit,*)nam2
      WRITE(ounit,'(a)')"*-species names-------------------"
      WRITE(ounit,'('//cnods//'(1x,a10))')nam2(1:nods)
      WRITE(ounit,'(a)')
     &      "*-annual emissions-(year, 20 x real columns)--------"
      CALL skip(iunit,eof)

! CHECK that emissions file inputs are in same order
! IF NOT, STOP
      DO isp=1,nods
        IF(INDEX(nam2(isp),nam1(isp)).NE.1)THEN
          PRINT*,"BASELINE species ....... PERTURBATION species "
          PRINT*,isp,nam1(isp),invals1(isp),nam2(isp),invals2(isp)
          PRINT*,"!! mismatch in species order !!"
          PRINT*,"!! EDIT the PERTURBATION input file !!"
          STOP
        ENDIF
      ENDDO

! read the file... year, emissions by species

      iyr = 0
      DO WHILE (.NOT. eof)
        iyr = iyr+1
        READ(iunit,*) pyr(iyr),pert(1:nods,iyr)

! convert perturbations to kT if necessary (based on "units" string)
        IF(units(1:1).EQ."k".OR.units(1:1).EQ."K")THEN
          ! no conversion needed
        ELSE
          pert(1:nods,iyr) = pert(1:nods,iyr) / 1e3
        ENDIF !(units(1:1).EQ."k".OR.units(1:1).EQ."K")THEN

! position file at next data line (ie not beginning with '*')
        CALL skip (iunit,eof)

      END DO !WHILE (.NOT. eof)

      CLOSE(iunit)

      npyr = iyr
      pyfirst = pyr(1)
      pylast = pyr(iyr)

! check alignment of years in emission and perturbation files
      IF(yfirst.NE.pyfirst.OR.ylast.NE.pylast)THEN
        PRINT*,"year ranges are in compatible"
        PRINT*,"Please add data for years:"
        PRINT*,yfirst,"-",pyfirst,"   and"
        PRINT*,ylast," -",pylast
        PRINT*,"to emissions and/or perturbation input files"
        STOP   
      ENDIF

!----------------------------------------------------------------------!
! ADD THE PERTURBATION TO THE EMISSIONS
!----------------------------------------------------------------------!
 
      DO iyr = 1,nyr
        emis(1:nods,iyr) = emis(1:nods,iyr) + pert(1:nods,iyr)
      ENDDO

!----------------------------------------------------------------------!
! GENERATE OUTPUT (scenario_a.EMI, on unit ounit)

      WRITE(logfile,*) 'Writing combined EMISSION output file ',filename

      DO iyr = 1,nyr
        WRITE(ounit,'(i4,20(1x,F9.3))') year(iyr),emis(1:nods,iyr)
      ENDDO

      WRITE(ounit,'(a)')"* END *"
      CLOSE(ounit)

!----------------------------------------------------------------------!
! end of subroutine
!----------------------------------------------------------------------!
      RETURN

! error conditions
999   PRINT*,"no such file : ",filename
      STOP

      END SUBROUTINE combine_emissions

!=====================================================================


