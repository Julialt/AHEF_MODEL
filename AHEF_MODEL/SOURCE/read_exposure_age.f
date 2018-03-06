C=====================================================================
      SUBROUTINE read_exposure_age(indexname)
C=====================================================================
C  Subroutine to read exposure by age file
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'

c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

!      LOGICAL eof
      CHARACTER*8 indexname, indextmp

      WRITE (errfile, *) 'Reading Exposure'

      REWIND(scratchage)
      CALL skip( scratchage, eof )

      DO WHILE (.NOT. eof)

      READ(scratchage,100)indextmp,colo_year
      READ(scratchage,103)cohi_year
c
100   FORMAT(t16,a8,t46,i4)
103   FORMAT(t25,i4)

        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

           DO icty = 1,numcty

           CALL skip( scratchage, eof )
            READ (scratchage,'(t18,i5)') cty(icty)
cc            WRITE(*,*)'cty = ',cty(icty)
          CALL skip( scratchage, eof )
cc mrlm - make sure lats aren't revered on read-in
cc 1/2008 - it's works fine!
cc      WRITE(91,*)ilat, lats(ilat)
cc mrlm - end
          DO icohort = colo, cohi

            READ(scratchage, 120) (expage(icohort,iagey,icty),
     +          iagey = 1, maxages * step + 4)
cc mrlm - added in for check
cc            WRITE(91,120) (expage(icohort,iagey,icty),
cc     +          iagey = 1, maxages * step + 4)
cc mlrm - end
c           WRITE(errfile, 120) (expage(icohort,iagey,ilat),
c    +          iagey = 1, maxages * step + 4)
120         FORMAT(t5,100(:,e12.4))

          ENDDO ! icohort
        ENDDO ! icty

c       WRITE (*,*) indexname,indextmp
        IF (indexname .EQ. indextmp) GOTO 999

        WRITE (errfile, *) 'Skipping to Next'

        CALL skip( scratchage, eof )

      ENDDO

      GOTO 1130

999   RETURN

1130  CALL error(130, *999)
 
      END SUBROUTINE read_exposure_age

