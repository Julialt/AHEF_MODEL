C=====================================================================
      SUBROUTINE readlookup(filename)
C=====================================================================
C   Read lookup table for given action spectrum
C   (i.e., irradiance by zenith angle and DObson units)
C=====================================================================
      IMPLICIT NONE
C$DEBUG: 'D'

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      INCLUDE 'exposure.fi'

      CHARACTER*12 filename
      INTEGER row,col,idummy
      LOGICAL eof

!      OPEN(iunit, file = filename,Defaultfile=
!     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data',
!c     +      'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\',
!     +status = 'OLD', err = 1040)
      OPEN(iunit, file = filename, status = 'OLD', err = 1040)
      WRITE(errfile,*) 'Reading LOOKUP'
c
c      mrlm 
cc      WRITE(*,*)'lrows, lcols = ',lrows,lcols
c
      CALL skip( iunit, eof )
      DO row = 1, lrows
        READ(iunit, *, err=1070) idummy,
     +                           (lookup(row,col), col=1, lcols)
      ENDDO

999   CLOSE (iunit)
      RETURN

1040  CALL error(40,*999)
1070  CALL error(70,*999)

      END SUBROUTINE readlookup

