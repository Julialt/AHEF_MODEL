C=====================================================================
      SUBROUTINE calc_irradiance
C=====================================================================
C  Calculate irradiance by year, month, and latitude
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'exposure.fi'
      INCLUDE 'setup.h'

c      mrlm day - make integer to match zenith routine!!!!
c
      REAL lat
      REAL daily,czen
      REAL dobunit

      INTEGER r1,r2,c1,c2
      REAL    row,col,dcol,drow,temp1,temp2
      REAL    lookup1,lookup2,dailyincr

! JMLT added 1 line
      CHARACTER(20) ctyfip_file

c - printing out for input into VBA
ccc--      REAL czenarr(17,12)
c

      yearlyirrad = 0                         ! initialize accumulator

      WRITE(errfile,*) 'Calculating Irradiance ...'
      WRITE(*,*)       'Calculating Irradiance ...'
c
c MRLM match county lat with latitude band to pick out the right ozone (DU)
c first read in county/latitude file

! JMLT added 1 line
! JMLT: Hardwired for DEBUG
      ctyfip_file = "test1/cntyfip_g1.txt"

! JMLT added argument
      CALL read_ctylat(ctyfip_file)
c
      DO icty = 1, numcty

! DEBUG !
!        PRINT*,"icty = ",icty
! END DEBUG !

cMRLM for each county, find the latitude
cJMLT previously approximated with AINT, now REAL
               lat = cty_lat(icty)

ccNEW - determine latitude region to use for dobson 
c
 801              IF ((lat.ge.20.).AND.(lat.le.30.)) THEN
                        ilat = 3
                  ELSEIF ((lat.ge.30.).AND.(lat.le.40.)) THEN
                        ilat = 2
                  ELSEIF ((lat.ge.40.).AND.(lat.le.50.)) THEN
                        ilat = 1
                  ENDIF
c
c            WRITE(*,*)'latitude,ilat = ',cty_lat(icty),ilat
cc            WRITE(917,*)'latitude of the county = ',cty_lat(icty)
cc            WRITE(917,*)'               integer = ',lat
cc            WRITE(917,*)'        latitude range = ',ilat
c
        DO iyear = yrlo, yrhi
c
cc      WRITE(*,*)' iyear in calc = ',iyear
c
          DO imonth = minmon, maxmon

            dobunit = dobson(iyear,imonth,ilat)
c MRLM added IF statement below 11/2014 - WMO files from 2013 are below 100
            IF (dobunit<100) dobunit = 100

! Find row, assuming irradiance lookup table goes from 100 to 600 DU step 10
            row = dobunit/10.0 - 9.0          
c
cc      WRITE (917,*)'iyear, imonth,dobunit = ',iyear,imonth,dobunit
cc      WRITE(917,*)' row = ',row
c
            IF ((drtype.EQ."CMPK").OR.(drtype.EQ."ANPK")) THEN
              iday = 21
            ELSE
              iday = 15                       ! Will approximate monthly
            ENDIF                            !  using midmonth * 30

            daily =  0

! loop plausible daylight hrs
            DO itime = 4, 20                   

! OLD              CALL zenith(lat,imonth,iday,itime,czen)
              CALL zenith(lat,czen)  ! NEW

! DEBUG !
!              WRITE(*,*)' cos zenith = ',iday,itime,czen
!              WRITE(*,*)'dobson',iyear,imonth,ilat,dobunit,row
!              WRITE(*,*)'zenith',iday,itime,lat,czen
! END DEBUG !
  
! IF daytime (i.e., cos(zen) > 0 i.e., 0 < zen < 90)
! Find column assuming lookup table goes by cosine, step 0.05
              IF (czen.GE.0.) THEN

                col = czen*20.0 + 1.0         

C=====================================================================
C  Real dobson and cos(zen) fall between values on lookup table axes
C  Interpolate in two dimensions to adjust
C=====================================================================
! OLD CODE
! lookup has issues:
! Type conversion of subscript expression for lookup
c      WRITE(*,*)'checks',r1,c1,c2
!                temp1 = lookup(r1,c1)+ 
!     &                 (lookup(r1,c2)-lookup(r1,c1)) * (col-c1)
!                temp2 = lookup(r2,c1)+ 
!     &                 (lookup(r2,c2)-lookup(r2,c1)) * (col-c1)

!                daily = daily+temp1 +(temp2-temp1) * (row-r1)
c
cc            WRITE(917,*)'daily = ',temp1 + (temp2 - temp1) * (row - r1),daily
c
!-------------------------------
! JMLT NEW CODE:
! Cell corner indices are (c1,r1),(C1,r2),(c2,r1),(c2,r2)
                c1 = IFIX(col)
                c2 = c1 + 1
                r1 = IFIX(row)
                r2 = r1 + 1

! Find position of obs. data relative to lookup table
                 dcol=(col-FLOAT(c1))
                 drow=(row-FLOAT(r1))

! Interpolate and increment
                 dailyincr = 
     &                   (dcol)   *(drow)   *lookup(r2,c2)+
     &                   (1.-dcol)*(drow)   *lookup(r2,c1)+
     &                   (dcol)   *(1.-drow)*lookup(r1,c2)+
     &                   (1.-dcol)*(1.-drow)*lookup(r1,c1)

                 daily=daily+dailyincr

! Values for checking
                 lookup1=AMIN1(lookup(r1,c1),lookup(r2,c1),
     &                         lookup(r1,c2),lookup(r2,c2))
                 lookup2=AMAX1(lookup(r1,c1),lookup(r2,c1),
     &                         lookup(r1,c2),lookup(r2,c2))

                 !WRITE(*,*)dobunit,czen,lookup1,dailyincr,lookup2,daily
                 !WRITE(*,*)dobunit,czen,dailyincr

              ENDIF ! (czen.GE.0)

! DEBUG OUTPUT !
              IF ((iyear.EQ.1985).AND.(icty.EQ.1).AND.(imonth.EQ.1).AND.
     +            (itime.EQ.12)) THEN
                WRITE(661,*)iyear,imonth,ilat,icty,cty_lat(icty),
     +                      dobunit,itime,czen,temp1,temp2,daily
              ENDIF ! selected sample point
! END DEBUG !

            ENDDO ! itime

            IF ((drtype.EQ."CMPK").OR.(drtype.EQ."ANPK")) THEN
               yearlyirrad(iyear,icty) = 
     &         yearlyirrad(iyear,icty) + daily
            ELSE
               yearlyirrad(iyear,icty) = 
     &         yearlyirrad(iyear,icty) + daily * 30.
            ENDIF
c

c
cc      WRITE(917,*)'yearlyirrad = ',yearlyirrad(iyear,icty),iyear,icty
c               
          ENDDO ! imonth

c      mrlm uncommented
cc      WRITE(917,*)'icty,iyear,yrirrad=',icty,iyear,yearlyirrad(iyear,icty)
c           WRITE(errfile,'(t1,i4,t6,i4,t12,e9.4)')
c     +          iyear, ilat, yearlyirrad(iyear,ilat)

       ENDDO ! iyear

      ENDDO ! icty
c
cc      WRITE(917,*)' ***'
c
      RETURN

      END SUBROUTINE calc_irradiance

