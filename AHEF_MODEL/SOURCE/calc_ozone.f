      SUBROUTINE calc_ozone(scenario)
!=====================================================================!
! PURPOSE: read EESC scenario and construct ozone distribution in 7
!          latitude bands: {70-60,60-50,50-40,40-30,30-20,NH,global}
! AUTHOR:  Julia Lee-Taylor, NCAR
! UPDATE: April 4, 2018
! NOTES: read data from files O3data_1980.TOMS, O3trend_1980-1990.TOMS
!        and WMO2010_BSL.ESC (ie scenario.ESC)
!=====================================================================!
! LIST OF SOME VARIABLES:
!
! mlat = total no. of latitude bands considered = 7 
!        { 70-60,60-50,50-40,40-30,30-20,NH,global }
! nati = # of columns of data in .ati files from vintaging model

! EESC = annual Effective Equivalent Stratospheric Chlorine release (pptv)

!--------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'global.h'
      INCLUDE 'files.h'
      INCLUDE 'setup.h'
  
      INTEGER,PARAMETER :: mlat=7  ! # of latitude bands 
      CHARACTER(1) :: cmlat="7"  ! # of latitude bands, string format

      INTEGER,PARAMETER :: myr=300 ! max # of years considered
      INTEGER,PARAMETER :: mmo=12  ! # of months in 1 year

      INTEGER,PARAMETER :: nati=25  ! # data in vintaging model *ati file
      INTEGER,PARAMETER :: nods=20  ! # data in vintaging model *ati file

!-----------------------
! input argument
      !CHARACTER(len=20),INTENT(IN) :: scenario
      CHARACTER(len=20) :: scenario

!-----------------------
! readin variables
      INTEGER,DIMENSION(myr) :: year
      REAL,DIMENSION(myr) :: eesc ! EESC integrated over all species
      REAL,DIMENSION(mmo,mlat) :: O3bsl ! O3(DU) in 1980 in all lat bands

!-----------------------
! output variables
      REAL,DIMENSION(myr,mmo,mlat) :: O3 ! output O3(DU) in all lat bands

!-----------------------
! internal variables
      INTEGER i,j,k,isp,imo,iyr,nyr,i1,i2
      REAL,DIMENSION(mlat) :: invals1
      REAL,DIMENSION(mmo,mlat) :: afac
      REAL :: bfac
      CHARACTER(10) adummy
      CHARACTER(20) bdummy
      CHARACTER(50) inpf, outf
      CHARACTER(len=50) :: filename

      CHARACTER(len=6),DIMENSION(mlat) :: latband =
     &                (/"70:60N","60:50N","50:40N","40:30N","30:20N",
     &                  "90N:EQ","Global"/)

      INTEGER,DIMENSION(mlat) :: lat1 = (/70,60,50,40,30,90,90/)
      INTEGER,DIMENSION(mlat) :: lat2 = (/60,50,40,30,20,0,-90/)

      CHARACTER(len=3),DIMENSION(mmo) :: month =
     &                (/"Jan","Feb","Mar","Apr","May","Jun",
     &                  "Jul","Aug","Sep","Oct","Nov","Dec"/)

!----------------------------------------------------------------------!
! INPUT FILENAMES
! "scenario" is now passed in from ATMOS.f
!      scenario = "WMO2010_BSL"

!----------------------------------------------------------------------!
! READ INPUT DATA: EESC
! col 1 = year; col 2 = total EESC; (optional: col 3+ = speciated EESC)

      i=INDEX(scenario," ")-1
      filename = scenario(1:i)//".ESC"

      OPEN(iunit,file=dir_esc//filename,status='OLD',err=999)

      WRITE(logfile,*) 'reading EESC file ',dir_esc//filename
      WRITE(*,*)       'reading EESC file ',dir_esc//filename

      CALL skip(iunit,eof)

      iyr = 0
      DO WHILE (.NOT. eof)
        iyr = iyr + 1

        READ(iunit,*) year(iyr),eesc(iyr) !,ODS_data(1:nods)%EESC(iyr)
        !PRINT*, year(iyr),eesc(iyr)

! position file at next data line (ie not beginning with '*')
        CALL skip (iunit,eof)

      ENDDO ! WHILE (.NOT. EOF)
      CLOSE(iunit)

! 'b' = diff EESC(scenario)(1990-1980) 

      nyr = iyr

      DO iyr = 1,nyr
        IF(year(iyr).EQ.1980) THEN
          j = iyr
          CYCLE
        ENDIF
        IF(year(iyr).EQ.1990) THEN
          k = iyr
          EXIT
        ENDIF
      ENDDO

      bfac = eesc(k)-eesc(j)

!----------------------------------------------------------------------!
! READ INPUT DATA: TOMS BASELINE (1980) = O3(1980) in equation below

      filename = "O3data_1980.TOMS"

      OPEN(iunit,file=dir_ozn//filename,status='OLD',err=999)

      WRITE(logfile,*) 'reading TOMS baseline file ',dir_ozn//filename
      WRITE(*,*)       'reading TOMS baseline file ',dir_ozn//filename

      CALL skip(iunit,eof)

      DO imo=1,mmo
        READ(iunit,*) i,invals1
        IF(i.EQ.imo)THEN
          O3bsl(imo,1:mlat) = invals1(1:mlat)
        ELSE
          PRINT*,"error in file ",filename,": month not found ",imo
        ENDIF
        CALL skip(iunit,eof)
      ENDDO

      CLOSE(iunit)

!----------------------------------------------------------------------!
! READ INPUT DATA: TOMS TREND, 1980-1990
! 'a' = diff O3(1990-1980) expressed as % (O3trend_1980-1990.TOMS)

      filename = "O3trend_1980-1990.TOMS"

      OPEN(iunit,file=dir_ozn//filename,status='OLD',err=999)

      WRITE(logfile,*) 'reading TOMS trend file ',dir_ozn//filename
      WRITE(*,*)       'reading TOMS trend file ',dir_ozn//filename

      CALL skip(iunit,eof)

      DO imo=1,mmo
        READ(iunit,*) i,invals1
        IF(i.EQ.imo)THEN
          afac(imo,:) = invals1(:)
        ELSE
          PRINT*,"error in file ",filename,": month not found ",imo
          STOP
        ENDIF
      ENDDO

      CLOSE(iunit)

      !print*,"afac = ",afac
      !print*,"bfac = ",bfac
!----------------------------------------------------------------------!
! CALCULATE O3 REGRESSION BASED ON EESC
! WMO(2011): O3(iyr) = O3(1980)+a/b*(EESC(iyr)-EESC(1980))

! O3(iyr) = O3(1980)+a/b*(EESC(iyr)-EESC(1980))
    
      DO iyr = 1,nyr
        DO imo = 1,mmo
          O3(iyr,imo,1:mlat) = O3bsl(imo,1:mlat) 
     &                       + afac(imo,1:mlat)/bfac
     &                       * (eesc(iyr)-eesc(j))
        ENDDO
       ENDDO


!----------------------------------------------------------------------!
! WRITE OZONE OUTPUT

      i=INDEX(scenario," ")-1
      filename = scenario(1:i)//".OZN"

      OPEN(ounit,file=dir_ozn//filename,status='UNKNOWN',err=999)

      WRITE(logfile,*) 'writing OZONE output file ',dir_ozn//filename
      WRITE(*,*)       'writing OZONE output file ',dir_ozn//filename

! write to the file
      WRITE(ounit,'(a)') "*  DOBSON(year,month,latitude)"
      WRITE(ounit,'(a)') "*  --"
      WRITE(ounit,'(a)') "* number of latitude bands:"
      WRITE(ounit,'(1x,i2,1x,a1,5x,'//cmlat//'(2x,a6))') 
     &                 mlat,':',(latband(i),i=1,mlat)
      WRITE(ounit,'(a)') "*  --"
      WRITE(ounit,'(1x,a8,'//cmlat//'(4x,i3,1x))') 
     &                "lat_hi = ",(lat1(i),i=1,mlat)
      WRITE(ounit,'(1x,a8,'//cmlat//'(4x,i3,1x))') 
     &                "lat_lo = ",(lat2(i),i=1,mlat)
      WRITE(ounit,'(a)') "* Yr:  Mo: "

      DO iyr = 1,nyr
        DO imo = 1,mmo
          WRITE(ounit,'(1x,i4,2x,a3,'//cmlat//'(2x,f6.2))') 
     &          year(iyr),month(imo),O3(iyr,imo,:)
        ENDDO
      ENDDO

      CLOSE(ounit)

!----------------------------------------------------------------------!
! end of program
!----------------------------------------------------------------------!
      RETURN

999   PRINT*,"no such file : ",filename
      stop


      END SUBROUTINE calc_ozone
      !END PROGRAM calc_ozone

!=====================================================================


