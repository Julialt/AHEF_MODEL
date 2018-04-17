      SUBROUTINE calc_EESC(odsfile,emisfile,scenario)
!=====================================================================!
! PURPOSE: read ODS emission scenario and calculate
!          Effective Equivalent Stratospheric Chlorine (EESC)
! AUTHOR:  Julia Lee-Taylor, NCAR
! UPDATE: April 2, 2018
! NOTES: 
!=====================================================================!
! LIST OF SOME VARIABLES:
!
! nods = total no. of Ozone Depleting Substances (ODS) = 20
! --  1: 5 = CFC-11, CFC-12, CFC-113, CFC-114, CFC-115
! --  6:10 = H-1211, H-1301, H-2402, CCl4, CH3CCl3
! -- 11:15 = HCFC-22, HCFC-123, HCFC-124, HCFC-141, HCFC-142
! -- 16:20 = HCFC-225ca, HCFC-225cb, CH3Cl, CH3Br, H-1202
!
! GENERAL ODS VARIABLE NAMES
! fsurf = factor to account for stratospheric profile slope
! fsurfCH3Br = (CH3Br skopes more steeply in strat)
! alpha = relative effectiveness of Br vs Cl at O3 destruction
!
! SPECIES-SPECIFIC VARIABLE NAMES
! tau  = Lifetime (yr)
! ex1  = (-1/tau)
! ncl  = number_Cl per species (integer)
! nbr  = number_Br per species (integer)
! wmol = molar weight of ODS
! emis = global emissions of ODS per year
! xnbr = equivalent strat. Cl release Fac. for Brominated Compounds
! cf   = emission -> burden conversion factor (kt/ppt)
! fclr = strat_Cl-Release factors
! fsurf =  (applied in type ODS_data) combinds fsurf and fsurfCH3Br
! trcl,trbr = annual tropospheric cl, br emission
! stcl,stbr = annual stratospheric cl, br release 
! ECEM = Equivalent Chlorine Emissions (pptv) (by year)
! EESC = Effective Equivalent Stratospheric Chlorine release (pptv) (by year)

! NEEDS: 
! define yfirst, ylast
! ingest standard info for filenames
! figure out factor of 1000.
! generate output EESC file
!--------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'global.fi'
      INCLUDE 'files.fi'
      INCLUDE 'setup.h'
  
      INTEGER,PARAMETER :: nods=20 ! # of O3-Depleting Substances
      INTEGER,PARAMETER :: myr=300 ! max # of years considered

!--------------------
! species_specific data and values

      TYPE :: species_data
        CHARACTER(10) :: spnam ! ODS name
        INTEGER :: ncl,nbr    ! readin: # Cl and Br
        REAL :: fclr,tau,wmol ! readin: frac Cl release, lifetime, molwt
        REAL :: xnbr,cf,ex1   ! equiv Cl release, kT/ppt, frac atm carryover
        REAL :: fsurf         ! factor for strat profile slope
        REAL,DIMENSION(myr) :: emis,conc
        REAL,DIMENSION(myr) :: ECEM  ! Cl + Equiv Cl in emissions (ppt)
        REAL,DIMENSION(myr) :: EESC  ! Cl + Equiv.Cl release in strat (ppt)
! could convert to internal real: dimension myr only needed for debugging output.
        REAL,DIMENSION(myr) :: trCl,trBr,stCl,stBr
      END TYPE species_data

      TYPE(species_data) :: ODS_data(nods)

! general ODS behavior parameters
      REAL :: fsurf, fsurfCH3Br, alpha

! units of emissions
      CHARACTER(10) :: units

! output values
      INTEGER,DIMENSION(myr) :: year,pyr
      REAL,DIMENSION(myr) :: eesctot ! EESC integrated over all species
!-----------------------
! standard atmospheric & molar parameters

      REAL,PARAMETER :: navo = 6.022140857E+23
      REAL,PARAMETER :: matm = 5.148E+18  ! kg
      REAL,PARAMETER :: atm_moles = matm*1e3 ! grammes
     &              / (0.72*2.*14.007 + 0.28*2.*15.999) ! / N2+O2 (g/mol)

!-----------------------
! internal variables
      INTEGER :: i,j,isp,iyr,tyr,i1,i2
      INTEGER :: yfirst,ylast,nyr,pyfirst,pylast,npyr
      INTEGER,DIMENSION(nods) :: invals1,invals2
      REAL,DIMENSION(nods,myr) :: tmpreal
      CHARACTER(len=50) :: filename,odsfile,emisfile,eescfile
      CHARACTER(len=20) :: scenario,scenario_p
      CHARACTER(len=10),DIMENSION(nods) :: tnam

!----------------------------------------------------------------------!
! initialise

      eesctot = 0
      DO i=1,nods
        ODS_data(i)%emis = 0.
      ENDDO

!----------------------------------------------------------------------!
! open and read ODS data file

      filename = dir_ods//odsfile
      OPEN(UNIT=iunit,FILE=filename,STATUS="OLD",ERR=999)
      WRITE(*,*)'Reading ODS data file ',filename

      CALL skip(iunit,eof)

      READ(iunit,*) fsurf, fsurfCH3Br, alpha
      !PRINT*, fsurf, fsurfCH3Br, alpha
      CALL skip(iunit,eof)

      isp = 0
      DO WHILE (.NOT. eof)
        isp = isp+1
   
! read in species data: name, ncl, nbr, fclr, tau, wmol
        READ(iunit,*) ODS_data(isp)%spnam 
     *               ,ODS_data(isp)%ncl
     *               ,ODS_data(isp)%nbr
     *               ,ODS_data(isp)%fclr
     *               ,ODS_data(isp)%tau
     *               ,ODS_data(isp)%wmol

! DEBUG
!        PRINT*,ODS_data(isp)%spnam 
!     *        ,ODS_data(isp)%ncl
!     *        ,ODS_data(isp)%nbr
!     *        ,ODS_data(isp)%fclr
!     *        ,ODS_data(isp)%tau
!     *        ,ODS_data(isp)%wmol
! END DEBUG

! position file at next data line (ie not beginning with '*')
        CALL skip (iunit,eof)

      END DO !WHILE (.NOT. eof)

      CLOSE(iunit)
!----------------------------------------------------------------------!
! open and read species emissions file

      filename = dir_ems//emisfile
      OPEN(UNIT=iunit,FILE=filename,STATUS="OLD",ERR=999)
      WRITE(*,*)'Reading EMISSION data file ',filename

      CALL skip(iunit,eof)

! read the header... units string (expecting "T.yr" or "kT/yr"
      READ(iunit,*) units
      CALL skip(iunit,eof)

! read the header... column numbers
      READ(iunit,*) invals1
      CALL skip(iunit,eof)

! read the header... species names
      READ(iunit,*)tnam
      CALL skip(iunit,eof)
     
! CHECK that emissions file and ODS file inputs are in same order
! IF NOT, STOP
      DO isp=1,nods
        IF(INDEX(tnam(isp),ODS_data(isp)%spnam).NE.1)THEN
          PRINT*,"ODS species ....... EMISSIONS species "
          PRINT*,isp,ODS_data(isp)%spnam ,invals1(isp),tnam(isp)
          PRINT*,"!! mismatch in species order !!"
          PRINT*,"!! EDIT the EMISSIONS input file !!"
          STOP
        ENDIF
      ENDDO

! read the file... year, emissions by species

      iyr = 0
      DO WHILE (.NOT. eof)
        iyr = iyr+1
  
        READ(iunit,*) year(iyr),ODS_data%emis(iyr)

! convert emissions to kT if necessary (based on "units" string)
        IF(units(1:1).EQ."k".OR.units(1:1).EQ."K")THEN
          ! no conversion needed
        ELSE
          ODS_data%emis(iyr) = ODS_data%emis(iyr) / 1e3
        ENDIF !(units(1:1).EQ."k".OR.units(1:1).EQ."K")THEN

! position file at next data line (ie not beginning with '*')
        CALL skip (iunit,eof)

      END DO !WHILE (.NOT. eof)

      CLOSE(iunit)

      nyr = iyr
      yfirst = year(1)
      ylast = year(iyr)

!----------------------------------------------------------------------!
! DO THE CALCULATIONS : DERIVED QUANTITIES
!----------------------------------------------------------------------!
 
! DERIVED SPECIES PROPERTIES : ex1 = EXP(-1/tau(isp))
      ODS_data%ex1 = EXP(-1/ODS_data%tau)

! DERIVED SPECIES PROPERTIES : xnbr = alpha * nbr(isp) 
      ODS_data%xnbr = alpha * ODS_data%nbr

! DERIVED SPECIES PROPERTIES : fsurf = fsurf or fsurfCH3Br as needed
      ODS_data%fsurf = fsurf
      WHERE(ODS_data%spnam.EQ."CH3Br") ODS_data%fsurf = fsurfCH3Br

! DERIVED SPECIES PROPERTIES : cf (kT/ppt) = atm_moles*molwt*1e-9*1e-12
! > atm_moles (total moles in atmos) = matm/(0.72*molwt(N2)+0.28*molwt(O2))
! > factor 1e-9 converts from g to kT (1 kT = 1 Gg)
! > factor 1e-12 converts from fraction to 1/ppt
      ODS_data%cf = ODS_data%wmol*atm_moles*1.e-21

!----------------------------------------------------------------------!
! CALCULATE ODS CONC (in pptv) FOR EACH YEAR : eqn pg 8, 2015 AHEF report
! first year

      iyr = 1

      ODS_data%conc(iyr) = (1-ODS_data%ex1) * ODS_data%emis(iyr) 
     &                   * ODS_data%tau * ODS_data%fsurf / ODS_data%cf

! subsequent years, including contribution from previous year

      j=ylast-yfirst+1

      DO iyr = 2,nyr
        ODS_data%conc(iyr) =  ODS_data%ex1 * ODS_data%conc(iyr-1) 
     &                     +  (1-ODS_data%ex1) * ODS_data%emis(iyr) 
     &                  *  ODS_data%tau * ODS_data%fsurf / ODS_data%cf
      ENDDO

! CALCULATE trBr = ANNUAL # of trop. Br in ODS expressed as Cl equiv
! CALCULATE trCl = ANNUAL # of tropospheric Cl in ODS
! CALCULATE ECEM = ANNUAL emissions OF Cl + EQUIV Cl (from Br)

      DO iyr = 1,nyr
        ODS_data%trBr(iyr) =  ODS_data%conc(iyr) * ODS_data%xnbr
        ODS_data%trCl(iyr) =  ODS_data%conc(iyr) * FLOAT(ODS_data%ncl)
        ODS_data%ECEM(iyr) =  ODS_data%trCl(iyr) + ODS_data%trBr(iyr) 
      ENDDO

! Considering a 3-year delay for transport to the stratosphere:
! CALCULATE stCl = ANNUAL Cl in ODS released in strat
! CALCULATE stBr = ANNUAL Br (expressed as Cl equiv) in ODS released in strat
! CALCULATE EESC = ANNUAL Cl + EQUIV Cl (from Br) released in strat 
! CALCULATE eesctot = ANNUAL strat Cl release integrated over all ODS

      DO iyr = 1,3
        ODS_data%stCl(iyr) = 0.
        ODS_data%stBr(iyr) = 0.
        ODS_data%EESC(iyr) = 0.
        DO i=1,nods
          eesctot(iyr) = eesctot(iyr) + ODS_data(i)%EESC(iyr)
        ENDDO
      ENDDO

      DO iyr = 4,nyr
        ODS_data%stCl(iyr) = ODS_data%trCl(iyr-3)*ODS_data%fClr
        ODS_data%stBr(iyr) = ODS_data%trBr(iyr-3)*ODS_data%fClr
        ODS_data%EESC(iyr) = ODS_data%stCl(iyr) + ODS_data%stBr(iyr) 
        DO i=1,nods
          eesctot(iyr) = eesctot(iyr) + ODS_data(i)%EESC(iyr)
        ENDDO
      ENDDO


!----------------------------------------------------------------------!
! DEBUG CHECKS
! (uncomment anything you want to print out)

      GOTO 600 ! to bypass debug section

      iyr = 65
      PRINT*,"EESC results for year :",year(iyr)

      DO isp=1,nods
        PRINT*,ODS_data(isp)%spnam
!     &        ,ODS_data(isp)%ncl       
!     &        ,ODS_data(isp)%nbr      
!     &        ,ODS_data(isp)%fclr    
!     &        ,ODS_data(isp)%wmol   
!     &        ,ODS_data(isp)%tau   
!     &        ,ODS_data(isp)%ex1    
!     &        ,ODS_data(isp)%fsurf 
!     &        ,ODS_data(isp)%cf  
!     &        ,ODS_data(isp)%xnbr     
!     &        ,ODS_data(isp)%emis(iyr)  
!     &        ,ODS_data(isp)%conc(iyr) 
!     &        ,ODS_data(isp)%trBr(iyr)
!     &        ,ODS_data(isp)%stBr(iyr)
!     &        ,ODS_data(isp)%trCl(iyr)
!     &        ,ODS_data(isp)%stCl(iyr)
!     &        ,ODS_data(isp)%ECEM(iyr)
     &        ,ODS_data(isp)%EESC(iyr)
      ENDDO
      PRINT*,"total EESC = " ,eesctot(iyr)

! END DEBUG CHECKS

600    CONTINUE

!----------------------------------------------------------------------!
! GENERATE OUTPUT ("eescfile")
! 1. total EESC; 2. speciated EESC

      i = INDEX(scenario," ")-1

      !PRINT*,scenario

      i = INDEX(scenario," ")-1
      eescfile = scenario(1:i)//".ESC"
      filename = eescfile

      OPEN(ounit,file=dir_esc//filename,status='UNKNOWN',err=999)

      WRITE(logfile,*) 'Writing EESC output file ',dir_esc//filename
      WRITE(*,*)       'Writing EESC output file ',dir_esc//filename

      WRITE(ounit,'(a)')"* EESC(ppt) from scenario "//scenario//" *"
      WRITE(ounit,'(a)')"* Year, total EESC, EESC per ODS ----->"

      DO iyr = 1,nyr
        WRITE(ounit,'(i4,21(1x,F8.2))')
     &               year(iyr),eesctot(iyr),ODS_data(1:nods)%EESC(iyr)
      ENDDO

      WRITE(ounit,'(a)')"* END *"
      CLOSE(ounit)

!----------------------------------------------------------------------!
! end of subroutine
!----------------------------------------------------------------------!
      !WRITE(*,*) " "
      RETURN

! error conditions
999   PRINT*,"no such file : ",filename
      STOP

      END SUBROUTINE calc_EESC

!=====================================================================


