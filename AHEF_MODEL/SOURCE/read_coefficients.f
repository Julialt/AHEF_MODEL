C=====================================================================
      SUBROUTINE read_coefficients
C=====================================================================
C   Read coefficients file
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.h'
      INCLUDE 'global.h'
      INCLUDE 'effects.h'

      INTEGER ilp
!---------------------------------------------------

      OPEN(iunit, file = coeffname, status = 'OLD', err = 1110)
      WRITE(logfile,*) 'Reading Coefficients'

      CALL skip( iunit, eof )

      DO ipop = 1, maxpops
        READ( iunit, 100, END=1070 ) (coeff(ipop,ilp), ilp=1,5)
        WRITE( logfile, 100 ) (coeff(ipop,ilp), ilp=1,5)
100     FORMAT(t15,f6.3,t25,100(f6.3,7x))

        CALL skip( iunit, eof )
      ENDDO

999   CLOSE (iunit)
      RETURN

1070  CALL error(70,*999)
1110  CALL error(110,*999)
      END SUBROUTINE read_coefficients


