C=====================================================================
      SUBROUTINE read_exposure_age(indexname)
C=====================================================================
C  Subroutine to read exposure by age file
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.h'
      INCLUDE 'global.h'
      INCLUDE 'effects.h'

      CHARACTER(len=*),INTENT(IN) :: indexname

      CHARACTER(len=8) :: indextmp
      CHARACTER(len=4) :: ayeartmp

      INTEGER :: unit
      REAL,DIMENSION(maxcohorts,topage+4,numcty) :: exptmp

!--------------------------------------------

      WRITE (logfile, *) 'Reading Exposure'


      IF (expblflag) THEN
        unit=scratchagebl
        WRITE (*, *) 'Read_exposure_age_multi : *.XBA'
      ELSE
        unit=scratchage 
        WRITE (*, *) 'Read_exposure_age_multi : *.XSA'
      ENDIF

      REWIND(unit)
      CALL skip(unit,eof)

      DO WHILE (.NOT.eof)

! if years entered in .X*A as integer type
!        READ(unit,101)indextmp 
!        READ(unit,102)ayeartmp
!          READ(ayeartmp,*)colo_year
!        READ(unit,103)ayeartmp
!          READ(ayeartmp,*)cohi_year

! if conversion required from character to integer type
        READ(unit,101)indextmp 
        READ(unit,102)ayeartmp
          READ(ayeartmp,*)colo_year
        READ(unit,103)ayeartmp
          READ(ayeartmp,*)cohi_year

        WRITE(logfile,*)indextmp,colo_year,cohi_year
c
100     FORMAT(t16,a8,t46,i4)
101     FORMAT(t16,a8)
102     FORMAT(t22,a4)
103     FORMAT(t25,a4)


        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

        DO icty = 1,numcty

          CALL skip( unit, eof )
          READ (unit,'(t18,i5)') cty(icty)
           !WRITE(*,*)'cty = ',cty(icty)
          CALL skip( unit, eof )

cc mrlm - make sure lats aren't revered on read-in
cc 1/2008 - it's works fine!
cc      WRITE(91,*)ilat, lats(ilat)
cc mrlm - end

          DO icohort = colo, cohi

            READ(unit, 120) 
     &          (exptmp(icohort,iagey,icty),iagey=1,maxages*step+4)

120         FORMAT(t5,100(:,e12.4))

c           WRITE(logfile,120) 
c     &          (exptmp(icohort,iagey,ilat),iagey=1,maxages*step+4)

          ENDDO ! icohort

        ENDDO ! icty

        IF (expblflag) THEN
          expagebl = exptmp
        ELSE
          expage = exptmp
        ENDIF

! DEBUG
        WRITE (*,*) indexname," ",indextmp
! DEBUG
        IF (indexname.EQ.indextmp) GOTO 999

        WRITE (logfile, *) 'Skipping to Next'

        CALL skip(unit,eof)

      ENDDO

      GOTO 1130
    
! if desired index found
999   RETURN

! if desired index not found
1130  CALL error(130, *999)
 
      END SUBROUTINE read_exposure_age

