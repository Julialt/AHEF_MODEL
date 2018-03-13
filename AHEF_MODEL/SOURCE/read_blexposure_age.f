C=====================================================================
      SUBROUTINE read_blexposure_age(indexname)
C=====================================================================
C  Subroutine to read baseline exposure age file
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'

      CHARACTER*8 indexname, indextmp
      CHARACTER*4 ayeartmp

!--------------------------------------------------------------

      WRITE (errfile, *) 'Reading Baseline Exposure'
      WRITE (*,*) 'Read_blexposure_age : *.XBA'

      REWIND(scratchagebl)
      CALL skip(scratchagebl,eof)

      DO WHILE (.NOT.eof)

! see read_exposure_age for explanation of read 
        READ(scratchagebl,101)indextmp
        READ(scratchagebl,102)ayeartmp
          READ(ayeartmp,*)colo_year
        READ(scratchagebl,103)ayeartmp
          READ(ayeartmp,*)cohi_year

        WRITE(errfile,*)indextmp,colo_year,cohi_year
c
100     FORMAT(t16,a8,t46,i4)
101     FORMAT(t16,a8)
102     FORMAT(t22,a4)
103     FORMAT(t25,a4)

        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

c        DO ilat = 1, numlats
        DO icty = 1,numcty

          CALL skip( scratchagebl, eof )
          READ (scratchagebl,'(t18,i5)') cty(icty)
      WRITE(*,*)' cty(icty) = ',cty(icty)
c
          CALL skip(scratchagebl,eof)

          DO icohort = colo, cohi

            READ(scratchagebl, 120) 
     &          (expagebl(icohort,iagey,icty),iagey=1,maxages*step+4)

ccmrlm 10/2008 debug - uncommented write statement below - put comment back
cc           WRITE(errfile, 120) 
cc     &          (expagebl(icohort,iagey,ilat),iagey=1,maxages*step+4)

120         FORMAT(t5,100(:,e12.4))

          ENDDO ! icohort

        ENDDO ! icty

! DEBUG !
        WRITE (*,*) indexname," ",indextmp
! END DEBUG !
        IF (indexname.EQ.indextmp) GOTO 999

        WRITE (errfile, *) 'Skipping to Next'

        CALL skip(scratchagebl,eof)

      ENDDO
      GOTO 1130

! if desired index found
999   RETURN

! if desired index not found
1130  CALL error(130, *999)

      END SUBROUTINE read_blexposure_age

