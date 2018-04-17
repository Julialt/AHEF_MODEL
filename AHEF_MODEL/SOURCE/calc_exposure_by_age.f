C=====================================================================
      SUBROUTINE calc_exposure_by_age
C=====================================================================
C  Subroutine calculates exposure by age from irradiance
C  by year.
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.h'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.h'
      INCLUDE 'global.h'
      INCLUDE 'exposure.h'

      INTEGER agelo, agehi, eage
      INTEGER yrlplo, yrlphi, yrmult, temp
      REAL agewght, wght_denom, wght

      WRITE(logfile,*) "Calculating exposure by age"
      WRITE(*,*) "Calculating exposure by age"
c
c      WRITE(*,*)'***** yrhi, yrlo = ',yrhi,yrlo
c
      DO icty = 1,numcty
        DO icohort = colo,cohi

          iyear = colo_year + (icohort - 1) * step - int(step/2)
c            mrlm - above is the midyear of 1885 to 1890, etc.

          DO iagey = 1, maxages * step + 4

            temp = max(min(iyear+iagey-1, yrhi),yrlo)
            expos_age(icohort,iagey,icty) = yearlyirrad(temp,icty)

c      WRITE(917,*)'icohort,iagey,icty,temp=',iagey,icohort,icty,temp
c      WRITE(917,*)'                    yearlyiradd =',
c     +      expos_age(icohort,iagey,icty)

          ENDDO ! iagey
        ENDDO ! icohort
      ENDDO ! icty

      RETURN

      END SUBROUTINE calc_exposure_by_age

