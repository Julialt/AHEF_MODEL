C=====================================================================
      SUBROUTINE read_exposure(indexname)
C=====================================================================
C  Subroutine to read exposure file
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'

c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

!      LOGICAL eof
      CHARACTER*8 indexname, indextmp

      WRITE (logfile, *) 'Reading Exposure'

      REWIND(scratch)
      CALL skip(scratch,eof)

      DO WHILE (.NOT.eof)

        READ(scratch,100) indextmp, colo_year
!      WRITE(*,*)'indextmp, colo_year = ',indextmp,colo_year
        READ(scratch,102) cohi_year
!        READ(scratch, 102) numlats, cohi_year
        CALL skip(scratch,eof)

100     FORMAT(t14,a,a8,a,t45,i4)
102     FORMAT(t23,a,i4)
!102     FORMAT(t19,i2,t46,i4)

        colo = 1
        cohi = (cohi_year-colo_year)/step + 1

!        DO ilat = 1, numlats
        DO icty = 1,numcty

!          READ (scratch,'(t18,f5.2)') lats(ilat)
          READ (scratch,'(t18,f5.2)') cty(icty)
          CALL skip(scratch,eof)

          DO icohort = colo,cohi

            READ(scratch,120) 
     &          (expage(icohort,iage,icty),iage=1,maxages*step+4)
!            WRITE(logfile, 120) (expos1(icohort,iage,ilat),
!    +            iage = 1, maxages)
120         FORMAT(t5,100(:,e12.4))

          END DO ! icohort
        END DO ! icty

!       WRITE (*,*) indexname,indextmp
        IF (indexname .eq. indextmp) GOTO 999

        WRITE (logfile, *) 'Skipping to Next'
        CALL skip(scratch,eof)

      END DO

      GOTO 1130

999   RETURN

1130  CALL error(130,*999)

      END SUBROUTINE read_exposure

