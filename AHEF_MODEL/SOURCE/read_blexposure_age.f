C=====================================================================
      SUBROUTINE read_blexposure_age(indexname)
C=====================================================================
C  Subroutine to read baseline exposure age file
C=====================================================================

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      INCLUDE 'effects.fi'

!      LOGICAL eof
      CHARACTER*8 indexname, indextmp

      WRITE (errfile, *) 'Reading Baseline Exposure'
cc mrlm 10/2008 debug
cc      WRITE(*,*)'scratchagebl - ',scratchagebl
cc
      REWIND(scratchagebl)
      CALL skip( scratchagebl, eof )

      DO WHILE (.NOT. eof)

        READ(scratchagebl,100)indextmp,colo_year
cc      WRITE(*,*)'in lo = ',indextmp,colo_year
        READ(scratchagebl,103)cohi_year
cc      WRITE(*,*)'hi = ',cohi_year
c
100     FORMAT(t16,a8,t46,i4)
103     FORMAT(t25,i4)

        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

c        DO ilat = 1, numlats
        DO icty = 1,numcty

          CALL skip( scratchagebl, eof )
          READ (scratchagebl,'(t18,i5)') cty(icty)
cc      WRITE(*,*)' cty(icty) = ',cty(icty)
c
          CALL skip( scratchagebl, eof )

          DO icohort = colo, cohi

            READ(scratchagebl, 120) (expagebl(icohort,iagey,icty),
     +          iagey = 1, maxages * step + 4)
ccmrlm 10/2008 debug - uncommented write statement below - put comment back
cc           WRITE(errfile, 120) (expagebl(icohort,iagey,ilat),
cc     +          iagey = 1, maxages * step + 4)
120         FORMAT(t5,100(:,e12.4))

          ENDDO ! icohort
        ENDDO ! icty

c       WRITE (*,*) indexname,indextmp
        IF (indexname .eq. indextmp) GOTO 999

        WRITE (errfile, *) 'Skipping to Next'

        CALL skip( scratchagebl, eof )

      ENDDO
      GOTO 1130

999   RETURN

1130  CALL error(130, *999)

      END SUBROUTINE read_blexposure_age

