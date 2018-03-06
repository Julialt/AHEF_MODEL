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
      INCLUDE 'exposure.fi'
      INCLUDE 'setup.h'

c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      CHARACTER*12 filename
      INTEGER irow,icol,idummy
!      LOGICAL eof

      WRITE(errfile,*) 'Reading LOOKUP',filename
      WRITE(*,*) 'Reading LOOKUP ',filename

!      OPEN(iunit, file = filename,Defaultfile=
!     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data',
!c     +      'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\',
!     +status = 'OLD', err = 1040)

      OPEN(iunit,file=dir_dat//filename,status='OLD',err=1040)

! lrows & lcols are defined in file global.fi
      CALL skip(iunit,eof)
      DO irow = 1,lrows
        READ(iunit,*,err=1070,end=999) 
     &           idummy,(lookup(irow,icol),icol=1,lcols)
        CALL skip(iunit,eof)
      ENDDO

999   CLOSE (iunit)
      RETURN

1040  CALL error(40,*999)
1070  CALL error(70,*999)

      END SUBROUTINE readlookup

