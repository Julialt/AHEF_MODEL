C=====================================================================
      SUBROUTINE read_exposure_age(indexname)
C=====================================================================
C  Subroutine to read exposure by age file
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'

      CHARACTER*8 indexname, indextmp
      CHARACTER*4 ayeartmp

!--------------------------------------------

      WRITE (errfile, *) 'Reading Exposure'
      WRITE (*, *) 'Read_exposure_age : *.XSA'

      REWIND(scratchage)
      CALL skip(scratchage,eof)

      DO WHILE (.NOT.eof)

! if years entered in .XSA as integer type
!        READ(scratchage,101)indextmp 
!        READ(scratchage,102)ayeartmp
!          READ(ayeartmp,*)colo_year
!        READ(scratchage,103)ayeartmp
!          READ(ayeartmp,*)cohi_year

! if conversion required from character to integer type
        READ(scratchage,101)indextmp 
        READ(scratchage,102)ayeartmp
          READ(ayeartmp,*)colo_year
        READ(scratchage,103)ayeartmp
          READ(ayeartmp,*)cohi_year

        WRITE(errfile,*)indextmp,colo_year,cohi_year
c
100     FORMAT(t16,a8,t46,i4)
101     FORMAT(t16,a8)
102     FORMAT(t22,a4)
103     FORMAT(t25,a4)


        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

        DO icty = 1,numcty

          CALL skip( scratchage, eof )
          READ (scratchage,'(t18,i5)') cty(icty)
           WRITE(*,*)'cty = ',cty(icty)
          CALL skip( scratchage, eof )

cc mrlm - make sure lats aren't revered on read-in
cc 1/2008 - it's works fine!
cc      WRITE(91,*)ilat, lats(ilat)
cc mrlm - end

          DO icohort = colo, cohi

            READ(scratchage, 120) 
     &          (expage(icohort,iagey,icty),iagey=1,maxages*step+4)

120         FORMAT(t5,100(:,e12.4))

c           WRITE(errfile,120) 
c     &          (expage(icohort,iagey,ilat),iagey=1,maxages*step+4)

          ENDDO ! icohort

        ENDDO ! icty

! DEBUG
        WRITE (*,*) indexname," ",indextmp
! DEBUG
        IF (indexname.EQ.indextmp) GOTO 999

        WRITE (errfile, *) 'Skipping to Next'

        CALL skip(scratchage,eof)

      ENDDO

      GOTO 1130
    
! if desired index found
999   RETURN

! if desired index not found
1130  CALL error(130, *999)
 
      END SUBROUTINE read_exposure_age

