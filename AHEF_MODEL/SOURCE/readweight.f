C=====================================================================
      SUBROUTINE readweight(filename)
C=====================================================================
C   Read age weighting function
C=====================================================================

C$DEBUG: 'D'

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      INCLUDE 'exposure.fi'

      CHARACTER*12 filename
      LOGICAL eof
      INTEGER row, col, idummy

      OPEN(iunit, file = filename, Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\'
c     +      'C:\Users\rawlings\Desktop\ahef\input data\',
     +,status = 'OLD', err = 1060)
      WRITE(errfile,*) 'Reading WEIGHTS'

      CALL skip( iunit, eof )
      DO row = 1, maxages
        READ(iunit, 100, err=1070) idummy,
     +                           (weights(row,col), col=1, maxages)
        WRITE(errfile,100) idummy, (weights(row,col), col=1, maxages)
100     FORMAT(i3,100(:,2x,f5.1))
      ENDDO

999   CLOSE (iunit)
      RETURN

1060  CALL error(60,*999)
1070  CALL error(70,*999)

      END SUBROUTINE readweight(filename)

