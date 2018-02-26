C=====================================================================
      SUBROUTINE readozone(filename)
C=====================================================================
C  Read Dobson Unit matrix (D.U. for given latitude and month)
C=====================================================================

C$DEBUG: 'D'

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      INCLUDE 'exposure.fi'

      LOGICAL eof
      CHARACTER*12 filename

      errflag = .false.
C MRLM ozone debugging 10/2008
cc       WRITE(*,*)'in readozone - filename is:',filename
C
      OPEN(iunit, file = filename, Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\'
c     +      'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\',
     + ,status = 'OLD', 
     + err = 1030)
      WRITE (errfile,*) 'Reading OZONE'

      CALL skip( iunit, eof)
cc      WRITE(*,*)'MADE IT'
      READ(iunit, *, err=1070) numlats
c mrlm WRITE out
cc      WRITE(*,*)'MADE IT 2'
cc      WRITE(*,*)'numlats',numlats
c
      WRITE(errfile,*)'numlats = ',numlats
c
      CALL skip( iunit, eof)
      READ(iunit,*) (lats(ilat), ilat = 1,numlats)

      CALL skip( iunit, eof)

      DO WHILE (.NOT. eof)

        DO imonth = 1, 12

cc       WRITE(errfile,*)'imonth = ',imonth
c for 110 FORMAT for no cntl.ozn
c      IF (filename.EQ.'NOCNTL.OZN') THEN 
c for the following, use 120 FORMAT
      IF ((filename.EQ.'MA98N2.OZN').OR.
     +    (filename.EQ.'MP1987.OZN').OR.
     + (filename.EQ.'MP2007.OZN').OR.
     + (filename.EQ.'MPAMEN.OZN').OR.
     + (filename.EQ.'MA98O2.OZN').OR.
     +       (filename.EQ.'WMOA1C.OZN').OR.
     +       (filename.EQ.'WMOMPC.OZN').OR.
     +       (filename.EQ.'WMONOC.OZN').OR.
     + (filename.EQ.'WMO_NP.OZN').OR.
     + (filename.EQ.'WMO_A1.OZN').OR.
c WMO baseline
     +     (filename.EQ.'BASELINE.OZN').OR.
c     +     (filename.EQ.'MA98NR.OZN').OR.
     +     (filename.EQ.'EP1NvB.OZN').OR.
     +    (filename.EQ.'MA98NU.OZN'))        THEN
cc            WRITE(*,*)'non control ozone'
ccx            READ(iunit,110,err=1070) iyear,adummy,
ccx     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
ccx         WRITE(errfile,110) iyear, adummy,
ccx     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
            READ(iunit,120,err=1070) iyear,adummy,
     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
         WRITE(errfile,120) iyear, adummy,
     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
      ENDIF
c commented out below - since using the WMO baseline
cx      IF (filename.EQ.'BASELINE.OZN') THEN
c      .OR.
c     +      (filename.EQ.'MA98N2.OZN')) THEN
c          READ(iunit,100,err=1070) iyear, adummy,
c            READ(iunit,*,err=1070) iyear, adummy,
c     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
c         WRITE(errfile,100) iyear, adummy,
c     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
c MRLM FOR TESTING!!!!!  REMOVE!!!!!  
c  100              FORMAT(1x,i4,a3,3(f6.2))
120            FORMAT(1x,i4,4x,a3,3(f13.3))
110       FORMAT (i4,8x,a3,3x,3(f6.2,2x))
  100       FORMAT (1x,i4,4x,a3,100(:,f12.2)) ! NOTE: for reading decimal
C This FORMAT statement is for scientific notation
c 100       FORMAT (1x,i4,4x,a3,100(:,e12.4))
c      ENDIF
          CALL skip( iunit, eof )

        ENDDO

        IF ((yrlo .LT. 1) .OR. (iyear .LT. yrlo)) THEN
          yrlo = iyear
        ENDIF
c      mrlm WRITE out iyear
c      WRITE(*,*)'iyear =',iyear
c

      ENDDO
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

999   CLOSE(iunit)
      RETURN

1030  CALL error(30,*999)
1070  CALL error(70,*999)

      END SUBROUTINE readozone
