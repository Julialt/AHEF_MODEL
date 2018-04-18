C=====================================================================
      SUBROUTINE read_ozone(filename)
C=====================================================================
C  Read Dobson Unit matrix (D.U. for given latitude and month)
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.h'
      INCLUDE 'global.h'
      INCLUDE 'exposure.h'
      INCLUDE 'setup.h'

      CHARACTER(len=*),INTENT(IN) :: filename

      INTEGER :: i
      REAL,DIMENSION(maxlats) :: dobtmp

      CHARACTER(len=1) ::  cnlat
      CHARACTER(len=3) ::  adummy
      CHARACTER(len=52) :: Defaultfile
!---------------------------------------------

      errflag = .false.

C MRLM ozone debugging 10/2008
cc       WRITE(*,*)'in read_ozone - filename is:',filename

      WRITE(logfile,*) 'Reading ozone : ',filename
      WRITE(6,*)       'Reading ozone : ',filename

      OPEN(iunit,file=dir_ozn//filename,status='OLD',err=1030)

      CALL skip( iunit, eof)
      READ(iunit,*,err=1070) numlats
      WRITE(logfile,*)'numlats = ',numlats
      WRITE(6,*)      'numlats',numlats

      WRITE(cnlat,'(i1)')numlats

      CALL skip( iunit, eof)
      READ(iunit,*)    (lats(ilat), ilat = 1,numlats)
      WRITE(logfile,*) (lats(ilat), ilat = 1,numlats)
      WRITE(*,*)       (lats(ilat), ilat = 1,numlats)

      CALL skip( iunit, eof)
      DO WHILE (.NOT. eof)
        !DO imonth = 1, 12  ! JMLT : now reading month from input file

c for 110 FORMAT for no cntl.ozn
c      IF (filename.EQ.'NOCNTL.OZN') THEN 
cc            WRITE(*,*)'non control ozone'
ccx            READ(iunit,110,err=1070) iyear,adummy,
ccx     +          (dobtmp(ilat), ilat=1, numlats)
c            DO i=1,12
c              IF(adummy.EQ.monthname(i)) THEN
c                imonth=i
c                EXIT
c              ENDIF
c            ENDDO
c            dobson(iyear,imonth,1:numlats)=dobtmp(1:numlats)
ccx         WRITE(logfile,110) iyear, adummy,
ccx     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
c      ENDIF


c for the following, use 120 FORMAT
          IF ((filename.EQ.'MA98N2.OZN').OR.
     +        (filename.EQ.'MA98NU.OZN').OR.
     +        (filename.EQ.'MA98O2.OZN').OR.
     +        (filename.EQ.'MP1987.OZN').OR.
     +        (filename.EQ.'MP2007.OZN').OR.
     +        (filename.EQ.'MPAMEN.OZN').OR.
     +        (filename.EQ.'WMOA1C.OZN').OR.
     +        (filename.EQ.'WMOMPC.OZN').OR.
     +        (filename.EQ.'WMONOC.OZN').OR.
     +        (filename.EQ.'WMO_NP.OZN').OR.
     +        (filename.EQ.'WMO_A1.OZN').OR.
     +        (filename.EQ.'BASELINE.OZN').OR.  ! WMO baseline
     +        (filename.EQ.'EP1NvB.OZN')     ) THEN

            !READ(iunit,'(1x,i4,3x,a3,'//cnlat//'(7x,f6.2))',err=1070) 
! JMLT, 2018: now using flexible-format read
            READ(iunit,*,err=1070,end=999) 
     &          iyear,adummy,(dobtmp(ilat),ilat=1,numlats)

            DO i=1,12
              IF(adummy.EQ.monthname(i)) THEN
                imonth=i
                EXIT
              ENDIF
            ENDDO
            dobson(iyear,imonth,1:numlats)=dobtmp(1:numlats)

! JMLT, 2018: write now allows variable # of lats 
!            WRITE(logfile,'(1x,i4,3x,a3,'//cnlat//'(7x,f6.2))') 
!     &          iyear,adummy,(dobson(iyear,imonth,ilat),ilat=1,numlats)
          ENDIF

100       FORMAT(1x,i4,4x,a3,100(:,f12.2)) ! decimal notation
101       FORMAT(1x,i4,4x,a3,100(:,e12.4)) ! exponent notation
102       FORMAT(1x,i4,4x,a3,100(:,es12.4)) ! scientific exponent notation 
110       FORMAT(i4,8x,a3,3x,3(f6.2,2x))
120       FORMAT(1x,i4,3x,a3,3(7x,f6.2))  ! read for 3x latitudes


          CALL skip( iunit, eof )
        !ENDDO ! imonth = 1-12

        IF ((yrlo .LT. 1) .OR. (iyear .LT. yrlo)) THEN
          yrlo = iyear
        ENDIF
c      mrlm WRITE out iyear
c      WRITE(*,*)'iyear =',iyear

c      mrlm test
c      WRITE(*,*)'iyear = ',iyear
c      WRITE(*,*)'imonth = ',imonth
c      WRITE(*,*)'ilat = ',ilat
c      WRITE(*,*)'dobson of 1,1,1',dobson(1985,1,1)
c      WRITE(*,*)'dobson of 1,12,1',dobson(1985,12,1)
c      WRITE(*,*)'dobson of 10,12,3',dobson(2050,12,3)
c
        IF ((yrhi .LT. 1) .OR. (iyear .GT. yrhi)) THEN
          yrhi = iyear
        ENDIF

      ENDDO

999   CLOSE(iunit)
      RETURN

1030  CALL error(30,*999)
1070  CALL error(70,*999)

      END SUBROUTINE read_ozone
