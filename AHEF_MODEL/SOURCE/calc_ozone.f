      !SUBROUTINE calc_ozone
      PROGRAM calc_ozone
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

      INCLUDE 'global.fi'
      INCLUDE 'files.fi'
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
      REAL,DIMENSION(mmo,mlat) :: afac,bfac
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
! "atmrunname" & "scenario" will eventually be passed in from AHEF.f

      atmrunname = "ATM_RUN.BSL"

      scenario = "WMO2010_BSL"

!----------------------------------------------------------------------!
! READ INPUT DATA: EESC

      i=INDEX(scenario," ")-1
      filename = scenario(1:i)//".ESC"

      OPEN(atmrun,file=dir_esc//filename,status='OLD',err=999)

      WRITE(logfile,*) 'reading EESC file ',filename
      WRITE(*,*)       'reading EESC file ',filename

      CALL skip(atmrun,eof)

      iyr = 0
      DO WHILE (.NOT. eof)
        iyr = iyr + 1

! col 1 = year; col 2 = total EESC; col 3+ = speciated EESC

        READ(atmrun,*) year(iyr),eesc(iyr) !,ODS_data(1:nods)%EESC(iyr)
        PRINT*, year(iyr),eesc(iyr)

! position file at next data line (ie not beginning with '*')
        CALL skip (atmrun,eof)

      ENDDO ! WHILE (.NOT. EOF)
      CLOSE(atmrun)

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
      print*,bfac

!----------------------------------------------------------------------!
! READ INPUT DATA: TOMS BASELINE (1980) = O3(1980) in equation below

      filename = "O3data_1980.TOMS"

      OPEN(atmrun,file=dir_ozn//filename,status='OLD',err=999)

      WRITE(logfile,*) 'reading TOMS baseline file ',filename
      WRITE(*,*)       'reading TOMS baseline file ',filename

      CALL skip(atmrun,eof)

      DO imo=1,mmo
        READ(atmrun,*) i,invals1
        IF(i.EQ.imo)THEN
          O3bsl(imo,1:mlat) = invals1(1:mlat)
        ELSE
          PRINT*,"error in file ",filename,": month not found ",imo
        ENDIF
      ENDDO

!----------------------------------------------------------------------!
! READ INPUT DATA: TOMS TREND, 1980-1990
! 'a' = diff O3(1990-1980) expressed as % (O3trend_1980-1990.TOMS)

      filename = "O3trend_1980-1990.TOMS"

      OPEN(atmrun,file=dir_ozn//filename,status='OLD',err=999)

      WRITE(logfile,*) 'reading TOMS trend file ',filename
      WRITE(*,*)       'reading TOMS trend file ',filename

      CALL skip(atmrun,eof)

      DO imo=1,mmo
        READ(atmrun,*) i,invals1
        IF(i.EQ.imo)THEN
          afac(imo,:) = invals1(:)
        ELSE
          PRINT*,"error in file ",filename,": month not found ",imo
        ENDIF
      ENDDO

!----------------------------------------------------------------------!
! CALCULATE O3 REGRESSION BASED ON EESC
! WMO(2011): O3(iyr) = O3(1980)+a/b*(EESC(iyr)-EESC(1980))

! O3(iyr) = O3(1980)+a/b*(EESC(iyr)-EESC(1980))
    
      DO iyr = 1,nyr
        DO imo = 1,mmo
          O3(iyr,imo,1:mlat) = O3bsl(imo,1:mlat) 
     &                       + afac(imo,1:mlat)/bfac(imo,1:mlat)
     &                       * (eesc(iyr)-eesc(j))
        ENDDO
       ENDDO


!----------------------------------------------------------------------!
! WRITE OZONE OUTPUT

      i=INDEX(scenario," ")-1
      filename = scenario(1:i)//".OZN"

      OPEN(atmrun,file=dir_ozn//filename,status='UNKNOWN',err=999)

      WRITE(logfile,*) 'writing OZONE output file ',filename
      WRITE(*,*)       'writing OZONE output file ',filename

! write to the file
      WRITE(atmrun,'(a)') "*  DOBSON(year,month,latitude)"
      WRITE(atmrun,'(a)') "*  --"
      WRITE(atmrun,'(a)') "* number of latitude bands:"
      WRITE(atmrun,'(1x,i2,1x,a1,5x,'//cmlat//'(2x,a6))') 
     &                 mlat,':',(latband(i),i=1,mlat)
      WRITE(atmrun,'(a)') "*  --"
      WRITE(atmrun,'(1x,a8,'//cmlat//'(4x,i3,1x))') 
     &                "lat_hi = ",(lat1(i),i=1,mlat)
      WRITE(atmrun,'(1x,a8,'//cmlat//'(4x,i3,1x))') 
     &                "lat_lo = ",(lat2(i),i=1,mlat)
      WRITE(atmrun,'(a)') "* Yr:  Mo: "

      DO iyr = 1,nyr
        DO imo = 1,mmo
          WRITE(atmrun,'(1x,i4,2x,a3,'//cmlat//'(2x,f6.2))') 
     &          year(iyr),month(imo),O3(iyr,imo,:)
        ENDDO
      ENDDO

      CLOSE(logfile)

!----------------------------------------------------------------------!
! end of program
!----------------------------------------------------------------------!
      RETURN

999   PRINT*,"no such file : ",filename
      stop


      !END SUBROUTINE calc_ozone
      END PROGRAM calc_ozone

!=====================================================================
      SUBROUTINE skip(iounit,eof)
C=====================================================================
C  Subroutine to skip comments while reading data files
C  Positions file at next data record by skipping those comment
C  records denoted by an asterisk in column 1.  The flag indicates
C  when the end of file has been reached.
C=====================================================================
      IMPLICIT NONE

! INCLUDING global.fi requires subroutine call with NO ARGUMENTS !
!      INCLUDE 'global.fi'    

      INTEGER iounit
      LOGICAL eof     ! only if global.fi not invoked
      CHARACTER*1 col1

      eof = .false.

100   READ(iounit,'(a1)',END=200) col1
! DEBUG
!      PRINT*,"skip:",col1
! END DEBUG
      IF (col1 .NE. '*') GOTO 300
      GOTO 100

200   eof = .true.

300   BACKSPACE iounit

      RETURN

      END SUBROUTINE skip


