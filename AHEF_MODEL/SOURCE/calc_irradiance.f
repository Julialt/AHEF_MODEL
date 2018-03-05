C=====================================================================
      SUBROUTINE calc_irradiance
C=====================================================================
C  Calculate irradiance by year, month, and latitude
C=====================================================================

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'exposure.fi'
c      mrlm day - make integer to match zenith routine!!!!
c
      INTEGER day, lat, ctfip
c
      REAL daily,czen
      REAL dobunit
c - printing out for input into VBA
ccc--      REAL czenarr(17,12)
c

      yearlyirrad = 0                         ! initialize accumulator

      WRITE (errfile,*) 'Calculating Irradiance ...'
c
c MRLM match county lat with latitude band to pick out the right ozone (DU)
c first read in county/latitude file
              call Read_CTYLAT
c
      DO icty = 1, numcty
c        latitude = lats(ilat)
c
cMRLM for each county, calculate the latitude
               lat = aint(cty_lat(icty))  ! from real to integer                     

ccNEW - determine latitude region to use for dobson 
c
 801              IF ((lat.ge.20).AND.(lat.le.30)) THEN
                        ilat = 3
                  ELSEIF ((lat.ge.30).AND.(lat.le.40)) THEN
                        ilat = 2
                  ELSEIF ((lat.ge.40).AND.(lat.le.50)) THEN
                        ilat = 1
                  ENDIF
c
cc            WRITE(917,*)'latitude of the county = ',cty_lat(icty)
cc            WRITE(917,*)'               integer = ',lat
cc            WRITE(917,*)'        latitude range = ',ilat
c
        DO iyear = yrlo, yrhi
c
cc      WRITE(*,*)' iyear in calc = ',iyear
c
          DO imonth = minmon, maxmon

c
            dobunit = dobson(iyear,imonth,ilat)
c MRLM added IF statement below 11/2014 - WMO files from 2013 are below 100
            IF (dobunit<100) THEN
                  dobunit = 100
            ENDIF 
            row = dobunit/10.0 - 9.0          ! Assumes lookup table goes
                                              !  from 100 to 600 DU step 10
c

c
cc      WRITE (917,*)'iyear, imonth,dobunit = ',iyear,imonth,dobunit
cc      WRITE(917,*)' row = ',row
c
            IF ((drtype.EQ."CMPK").OR.(drtype.EQ."ANPK")) THEN
              day = 21
            ELSE
              day = 15                       ! Will approximate monthly
            ENDIF                            !  using midmonth * 30

            daily =  0

            DO time = 4, 20                   ! loop plausible daylight hrs
c
                  call zenith(lat,imonth,day,time,czen)
c
cc      WRITE(917,*)' cos zenith = ',day,time,czen
c
c              call zenith(latitude, imonth, day, time, czen)
c
cc            WRITE(*,*)'dobson',iyear,imonth,ilat,dobunit,row
cc            WRITE(*,*)'zenith',day,time,lat,czen
c
              IF (czen .ge. 0) THEN           ! daytime i.e., cos(zen) > 0
                                              !         i.e., 0 < zen < 90
                col = czen*20.0 + 1.0         ! Assumes lookup table goes
                                              ! by cosine, step 0.05

C=====================================================================
C  Real dobson and cos(zen) fall between values on lookup table axes
C  Interpolate in two dimensions to adjust
C=====================================================================

                clow = aint(col)
                chigh = clow + 1
                rlow = aint(row)
                rhigh = rlow + 1
c      WRITE(*,*)'checks',rlow,clow,chigh
!                temp1 = lookup(rlow,clow) + (lookup(rlow,chigh) -
!     +                  lookup(rlow,clow)) * (col - clow)
!                temp2 = lookup(rhigh,clow) + (lookup(rhigh,chigh) -
!     +                  lookup(rhigh,clow)) * (col - clow)
! JMLT TEMPORARY FIX:
! lookup has issues:
! Type conversion of subscript expression for lookup

                temp1 = 1
                temp2 = 1

                daily = daily +
     +                  temp1 + (temp2 - temp1) * (row - rlow)
              ENDIF
c
cc            WRITE(917,*)'daily = ',temp1 + (temp2 - temp1) * (row - rlow),daily
c
      IF ((iyear.EQ.1985).AND.(icty.EQ.1).AND.(imonth.EQ.1).AND.
     +   (time.EQ.12)) THEN
      WRITE(661,*)iyear,imonth,ilat,icty,cty_lat(icty),dobunit,
     +   time,czen,temp1,temp2,daily
      ENDIF
            ENDDO ! time

            IF ((drtype.EQ."CMPK").OR.(drtype.EQ."ANPK")) THEN
              yearlyirrad(iyear,icty) = yearlyirrad(iyear,icty) +
     +                                  daily
              ELSE
              yearlyirrad(iyear,icty) = yearlyirrad(iyear,icty) +
     +                                  daily * 30
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

