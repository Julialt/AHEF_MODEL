C====================================================================================
      SUBROUTINE read_ctylat(filename)
C====================================================================================
c  MRLM - to read in county file with state/coounty fip and the associated latitude
c  at the county centroid location
C====================================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'exposure.fi'
      INCLUDE 'setup.h'

      integer icty
      CHARACTER*(*) filename
c
c
      WRITE(errfile,*) "Reading ctyfip file ",filename
      OPEN(unit=67,file=dir_io//filename, status="old")
c
      CALL skip(67, eof )
c
      icty = 0
      DO icty = 1, maxcty  ! or while not end of file
        !READ(67,188)cty_fip(icty),cty_lat(icty)
        READ(67,*,end=999) cty_fip(icty),cty_lat(icty)
        CALL skip(67, eof )

c FOR ONE COUNTY RUN
!      IF (cty_fip(icty).EQ.1001) THEN
!       WRITE(*,*)' right one = ',cty_lat(icty)
!      ELSE
!        CYCLE
!      ENDIF
c          
 188      FORMAT(I5,1x,f9.6)
c
      ENDDO

 999  CLOSE (67)
c
      RETURN 

      END SUBROUTINE read_ctylat
            
