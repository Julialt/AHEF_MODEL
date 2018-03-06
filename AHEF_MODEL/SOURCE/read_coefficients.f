C=====================================================================
      SUBROUTINE read_coefficients
C=====================================================================
C   Read coefficients file
C=====================================================================

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      INCLUDE 'effects.fi'

!      LOGICAL eof
      INTEGER ilp

      OPEN(iunit, file = coeffname, status = 'OLD', err = 1110)
      WRITE(errfile,*) 'Reading Coefficients'

      CALL skip( iunit, eof )

      DO ipop = 1, maxpops
        READ( iunit, 100, END=1070 ) (coeff(ipop,ilp), ilp=1,5)
        WRITE( errfile, 100 ) (coeff(ipop,ilp), ilp=1,5)
100     FORMAT(t15,f6.3,t25,100(f6.3,7x))

        CALL skip( iunit, eof )
      ENDDO

999   CLOSE (iunit)
      RETURN

1070  CALL error(70,*999)
1110  CALL error(110,*999)
      END SUBROUTINE read_coefficients


