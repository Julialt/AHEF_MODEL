C=====================================================================
      SUBROUTINE CALC_IRRADIANCE
C=====================================================================
C  Calculate irradiance by year, month, and latitude
C=====================================================================

      include 'files.fi'
      include 'global.fi'
      include 'exposure.fi'
c	mrlm day - make integer to match zenith routine!!!!
c
	integer day, lat, ctfip
c
      real daily,czen
      real dobunit
c - printing out for input into VBA
ccc--	real czenarr(17,12)
c

      yearlyirrad = 0                         ! initialize accumulator

      write (errfile,*) 'Calculating Irradiance ...'
c
c MRLM match county lat with latitude band to pick out the right ozone (DU)
c first read in county/latitude file
		  call Read_CTYLAT
c
      do icty = 1, numcty
c        latitude = lats(ilat)
c
cMRLM for each county, calculate the latitude
		   lat = aint(cty_lat(icty))  ! from real to integer			   

ccNEW - determine latitude region to use for dobson 
c
 801			if ((lat.ge.20).and.(lat.le.30)) then
				ilat = 3
			elseif ((lat.ge.30).and.(lat.le.40)) then
				ilat = 2
			elseif ((lat.ge.40).and.(lat.le.50)) then
				ilat = 1
			end if
c
cc		write(917,*)'latitude of the county = ',cty_lat(icty)
cc		write(917,*)'               integer = ',lat
cc		write(917,*)'        latitude range = ',ilat
c
        do iyear = yrlo, yrhi
c
cc	write(*,*)' iyear in calc = ',iyear
c
          do imonth = minmon, maxmon

c
            dobunit = dobson(iyear,imonth,ilat)
c MRLM added if statement below 11/2014 - WMO files from 2013 are below 100
            if (dobunit<100) then
	            dobunit = 100
	      endif 
            row = dobunit/10.0 - 9.0          ! Assumes lookup table goes
                                              !  from 100 to 600 DU step 10
c

c
cc	write (917,*)'iyear, imonth,dobunit = ',iyear,imonth,dobunit
cc	write(917,*)' row = ',row
c
            if ((drtype.eq."CMPK").or.(drtype.eq."ANPK")) then
              day = 21
            else
              day = 15                       ! Will approximate monthly
            endif                            !  using midmonth * 30

            daily =  0

            do time = 4, 20                   ! loop plausible daylight hrs
c
			call zenith(lat,imonth,day,time,czen)
c
cc	write(917,*)' cos zenith = ',day,time,czen
c
c              call zenith(latitude, imonth, day, time, czen)
c
cc		write(*,*)'dobson',iyear,imonth,ilat,dobunit,row
cc		write(*,*)'zenith',day,time,lat,czen
c
              if (czen .ge. 0) then           ! daytime i.e., cos(zen) > 0
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
c	write(*,*)'checks',rlow,clow,chigh
                temp1 = lookup(rlow,clow) + (lookup(rlow,chigh) -
     +                  lookup(rlow,clow)) * (col - clow)
                temp2 = lookup(rhigh,clow) + (lookup(rhigh,chigh) -
     +                  lookup(rhigh,clow)) * (col - clow)

                daily = daily +
     +                  temp1 + (temp2 - temp1) * (row - rlow)
              endif
c
cc		write(917,*)'daily = ',temp1 + (temp2 - temp1) * (row - rlow),daily
c
	if ((iyear.eq.1985).and.(icty.eq.1).and.(imonth.eq.1).and.
     +   (time.eq.12)) then
	write(661,*)iyear,imonth,ilat,icty,cty_lat(icty),dobunit,
     +   time,czen,temp1,temp2,daily
	end if
            end do ! time

            if ((drtype.eq."CMPK").or.(drtype.eq."ANPK")) then
              yearlyirrad(iyear,icty) = yearlyirrad(iyear,icty) +
     +                                  daily
	        else
              yearlyirrad(iyear,icty) = yearlyirrad(iyear,icty) +
     +                                  daily * 30
            endif
c

c
cc	write(917,*)'yearlyirrad = ',yearlyirrad(iyear,icty),iyear,icty
c		   
          end do ! imonth

c	mrlm uncommented
cc	write(917,*)'icty,iyear,yrirrad=',icty,iyear,yearlyirrad(iyear,icty)
c           write(errfile,'(t1,i4,t6,i4,t12,e9.4)')
c     +          iyear, ilat, yearlyirrad(iyear,ilat)

       end do ! iyear

      end do ! icty
c
cc	write(917,*)' ***'
c
      return
      end


