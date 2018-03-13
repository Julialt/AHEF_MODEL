C=====================================================================
      SUBROUTINE readweight(filename)
C=====================================================================
C   Read age weighting function
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'exposure.fi'

      CHARACTER*12 filename
      INTEGER row, col, idummy
!---------------------------------------------------------

      OPEN(iunit, file = filename,status = 'OLD', err = 1060)
      WRITE(errfile,*) 'Reading WEIGHTS ',filename
      WRITE(*,*)       'Reading WEIGHTS ',filename

      CALL skip( iunit, eof )
      DO row = 1, maxages
        READ(iunit,100,err=1070) idummy,
     +                           (weights(row,col), col=1, maxages)
        WRITE(errfile,100) idummy,(weights(row,col), col=1, maxages)
100     FORMAT(i3,100(:,2x,f5.1))
      ENDDO

999   CLOSE (iunit)
      RETURN

1060  CALL error(60,*999)
1070  CALL error(70,*999)

      END SUBROUTINE readweight

