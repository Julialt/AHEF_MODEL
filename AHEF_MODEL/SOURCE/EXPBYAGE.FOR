C=====================================================================
	SUBROUTINE CALC_EXPOSURE_BY_AGE
C=====================================================================
C  Subroutine calculates exposure by age from irradiance
C  by year.
C=====================================================================

      include 'files.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
	include 'global.fi'
      include 'exposure.fi'

      integer agelo, agehi, eage
      integer yrlplo, yrlphi, yrmult, temp
      real agewght, wght_denom, wght
c
c	write(*,*)'***** yrhi, yrlo = ',yrhi,yrlo
c
	do icty = 1, numcty

        do icohort = colo, cohi

          iyear = colo_year + (icohort - 1) * step - int(step/2)
c		mrlm - above is the midyear of 1885 to 1890, etc.
c

          do iagey = 1, maxages * step + 4

            temp = max(min(iyear+iagey-1, yrhi),yrlo)

c
		   expos_age(icohort,iagey,icty) = yearlyirrad(temp,icty)
c
c	write(917,*)'icohort,iagey,icty,temp=',iagey,icohort,icty,temp
c	write(917,*)'                    yearlyiradd =',
c     +	expos_age(icohort,iagey,icty)
c
          end do ! iagey

        end do ! icohort

      end do ! icty



      return
      end

